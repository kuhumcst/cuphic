(ns cuphic.core
  "Data transformations for Hiccup."
  (:require [clojure.spec.alpha :as s]
            [clojure.zip :as zip]
            [hickory.zip :as hzip]
            [lambdaisland.deep-diff2 :as dd]
            #?(:cljs [lambdaisland.deep-diff2.diff-impl :refer [Mismatch
                                                                Deletion
                                                                Insertion]])
            [cuphic.spec :as cs]
            [cuphic.symbols :as syms]
            [cuphic.zip :as czip])
  #?(:clj (:import [lambdaisland.deep_diff2.diff_impl Mismatch
                                                      Deletion
                                                      Insertion])))

;; TODO: replace brittle deep-diff2, sometimes different diffs in clj vs. cljs
(defn- attr-bindings
  "Get the symbol->value mapping found when comparing `cattr` to `hattr`.
  Returns nil if the two attrs don't match."
  [cattr hattr]
  (let [diffs (dd/diff cattr hattr)]
    (loop [loc (czip/vector-map-zip diffs)
           ret {}]
      (if (zip/end? loc)
        ret
        (let [{:keys [+ -] :as node} (zip/node loc)]
          (condp instance? node
            Deletion
            nil

            ;; Insertions are problematic unless they are HTML attributes.
            Insertion
            (when (and (keyword? +)
                       ;; Check if a keyword Insertion is inside a map.
                       (map? (zip/node (zip/up (zip/up loc)))))
              (recur (zip/next loc) ret))

            ;; Mismatches can indicate matching logic variables.
            Mismatch
            (when (symbol? -)
              (if (s/valid? ::cs/? -)
                (recur (zip/next loc) (assoc ret - +))
                (recur (zip/next loc) ret)))

            (recur (zip/next loc) ret)))))))

(declare get-bindings)

(defn- node-bindings
  "Return a potential direct binding between `pnode` and `node`."
  [pnode node]
  (cond
    (= pnode node)
    {}

    (syms/wildcard? pnode)
    {}

    (syms/variable? pnode)
    {pnode node}

    (and (vector? pnode) (vector? node))
    (get-bindings pnode node)

    (map? pnode)
    (attr-bindings pnode node)))

(defn- section-bindings
  "Get direct bindings for `pnodes` in `nodes` - or nil if they don't match.

  Since this compares every pnode to a corresponding node, the length of the two
  collections must be identical."
  [pnodes nodes]
  (when (= (count pnodes) (count nodes))
    (->> (map vector pnodes nodes)
         (reduce (fn [m [pnode node]]
                   (if-let [delta (node-bindings pnode node)]
                     (merge m delta)
                     (reduced nil)))
                 {}))))

(defn- section-search
  "Return direct bindings for the first occurrence of `pnodes` in `nodes`."
  [pnodes nodes]
  (let [n (count pnodes)]
    (loop [i 0]
      (let [nodes (take n (drop i nodes))]
        (when (>= (count nodes) n)
          (if-let [delta (section-bindings pnodes nodes)]
            (with-meta delta
                       {:from i
                        :to   (+ i n)})
            (recur (inc i))))))))

(defn- min-size
  [pnodes]
  (count (filter (complement syms/optional?) pnodes)))

;; TODO: probably not very efficient, make faster
(defn- concat-deltas
  [deltas]
  (reduce (fn [m delta]
            (apply merge-with into m (for [[k v] delta] {k [v]})))
          {}
          deltas))

(def ^:private normalise
  (memoize (fn [coll]
             (if (map? (second coll))
               coll
               (into [(first coll) {}] (rest coll))))))

(def ^:private fixed-length?
  (memoize (partial every? (complement syms/quantified?))))

;; TODO: repeated patterns cannot themselves contain quantifiers - reconsider?
(defn- repetition-bindings
  "Get bindings in `nodes` for a `pnode` containing a repeated pattern.

  Will bind successively until the fixed-length pattern no longer matches."
  [pnode nodes]
  (let [pattern (rest pnode)
        size    (count pattern)
        parts   (partition size nodes)
        deltas  (->> (map (partial section-bindings pattern) parts)
                     (remove nil?))]
    (when (not (and (syms/some-repeated? pnode)
                    (empty? deltas)))
      (with-meta (concat-deltas deltas)
                 {:from 0
                  :to   (* size (count deltas))}))))

(defn- arbitrary-bindings
  "Get bindings in `nodes` for an arbitrary section of `pnodes`.

  This will match all of the nodes and then destructure into individual parts:
  wildcards, variables, omissions, repetitions. If the number of nodes does not
  match the arity of the potential quantifier after destructuring, the bindings
  will be nil."
  [pnodes nodes]
  (loop [[pnode & pnodes] pnodes
         [node & nodes] nodes
         qnode    nil
         bindings {}]
    ;; Match bindings until the section is exhausted.
    (if pnode
      (cond
        (syms/wildcard? pnode)
        (recur pnodes nodes qnode
               (if qnode
                 (update bindings qnode rest)
                 bindings))

        (syms/variable? pnode)
        (recur pnodes nodes qnode
               (if qnode
                 (let [[node & stack] (get bindings qnode)]
                   (assoc bindings
                     pnode node
                     qnode stack))
                 (assoc bindings
                   pnode node)))

        ;; A quantifier will immediately capture any remaining nodes in a stack.
        ;; Subsequent wildcards or variables will each pop a node off the stack.
        (syms/quantified? pnode)
        (recur (reverse pnodes) nil pnode
               (assoc bindings
                 pnode (when node
                         (into `(~node) nodes)))))

      ;; Return bindings when both colls are exhausted.
      ;; The temporary qnode key is dissoc'ed, with potential bound variables
      ;; assoc'ed under their appropriate keys instead.
      (when (not node)
        (if qnode
          (if (syms/omitted? qnode)
            (if (syms/optional? qnode)
              (dissoc bindings qnode)
              (when (not-empty (get bindings qnode))
                (dissoc bindings qnode)))
            (let [stack (get bindings qnode)
                  delta (repetition-bindings qnode (reverse stack))]
              (when (= (-> delta meta :to) (count stack))
                (-> bindings
                    (dissoc qnode)
                    (merge delta)))))
          bindings)))))

(defn get-bindings
  "Find bindings in `hiccup` for a matching `pattern` - or nil on bad matches."
  [pattern hiccup]
  (let [pattern      (normalise pattern)
        hiccup       (normalise hiccup)
        section-type #(cond
                        (syms/arbitrary? %) :arbitrary
                        (syms/repeated? %) :repeated
                        :else :other)
        sections     (partition-by section-type pattern)
        arbitrary?   (comp syms/arbitrary? first)
        repeated?    (comp syms/repeated? first)]
    (loop [[pnodes & sections] sections
           [node :as nodes] hiccup
           bindings {}]
      (if pnodes
        (cond
          ;; Variable-length, arbitrary sections...
          (arbitrary? pnodes)
          ;; ... either match lazily until the next pattern...
          (if-let [next-section (first sections)]
            (let [next-section (if (repeated? next-section)
                                 (rest next-section)
                                 next-section)
                  skip         (min-size pnodes)
                  next-nodes   (drop skip nodes)]
              (when-let [next-delta (section-search next-section next-nodes)]
                (let [n (+ skip (:from (meta next-delta)))]
                  (when-let [delta (arbitrary-bindings pnodes (take n nodes))]
                    (recur sections (drop n nodes) (merge bindings delta))))))

            ;; ... or until the end (if this is the last pattern in the stack).
            (when-let [delta (arbitrary-bindings pnodes nodes)]
              (recur sections nil (merge bindings delta))))

          ;; TODO: should "destructure" an equivalent next section
          ;; Non-arbitrary, repeated patterns bind while the pattern matches.
          (repeated? pnodes)
          (when-let [delta (repetition-bindings (first pnodes) nodes)]
            (let [{:keys [to]} (meta delta)]
              (recur sections (drop to nodes) (merge bindings delta))))

          ;; Any other sections simply bind directly to nodes.
          :else
          (let [n (count pnodes)]
            (when-let [delta (section-bindings pnodes (take n nodes))]
              (recur sections (drop n nodes) (merge bindings delta)))))

        ;; Return bindings once both sections and nodes are both exhausted.
        (when (not node)
          (with-meta bindings {:source hiccup}))))))

(defn matches
  "Returns the match, if any, of `hiccup` to a Cuphic `pattern`."
  [pattern hiccup]
  (when (get-bindings pattern hiccup)
    hiccup))

(defn- ->repetition-bindings-fn
  "Helper fn for apply-bindings taking a `bindings` map and returning an fn that
  acts as (partial get bindings), but in a way such that collections only return
  the first item while swapping in the rest.

  Calling the returned fn with :done? will return 'true' if a collection has
  been exhausted.

  Calling the returned fn with :quantified? will return 'true' if a collection
  has been found among the returned values."
  [bindings]
  (let [bindings (atom bindings)]
    (fn [k]
      (let [v (get @bindings k)]
        (if (coll? v)
          (do
            (swap! bindings assoc :quantified? true)
            (when (<= (count v) 1)
              (swap! bindings assoc :done? true))
            (swap! bindings update k rest)
            (first v))
          v)))))

;; TODO: undefined behaviour for omitted content, wildcards, not found vars
(defn apply-bindings
  "Apply `bindings` to a Cuphic `pattern`.

  Quantified sub-patterns repeat until a quantified variable is exhausted.
  If the pattern does not contain any quantified variables, it runs only once."
  ([bindings pattern]
   (->> (czip/vector-map-zip pattern)
        (czip/reduce-zipper
          (fn [loc pnode]
            (if (syms/repeated? pnode)
              ;; A special bindings fn is used to delimit repeated content.
              ;; The same helper fn is also used to
              (let [bindings (->repetition-bindings-fn bindings)
                    pattern  (vec (rest pnode))]
                (loop [nodes []]
                  (if (bindings :done?)
                    (czip/multi-replace loc nodes)
                    (let [nodes (into nodes (apply-bindings bindings pattern))]
                      (if (bindings :quantified?)
                        (recur nodes)
                        (czip/multi-replace loc nodes))))))
              (if-let [replacement (bindings pnode)]
                (zip/replace loc replacement)
                loc)))))))

(defn transform
  "Transform `hiccup` using Cuphic `from-pattern` and `to-pattern`.

  Substitutes symbols in `to-pattern` with bound values from `hiccup` based on
  symbols in `from-pattern`. The Cuphic patterns can also be replaced with
  functions that either produce or consume a symbol->value map. "
  [from-pattern to-pattern hiccup]
  (when-let [bindings (if (fn? from-pattern)
                        (from-pattern hiccup)
                        (get-bindings from-pattern hiccup))]
    (if (fn? to-pattern)
      (to-pattern bindings)
      (apply-bindings bindings to-pattern))))

(defn ->transformer
  "Make a transformer to transform Hiccup using `from-pattern` and `to-pattern`.

  The returned fn will return the transformed value on successful matches and
  nil otherwise."
  [from-pattern to-pattern]
  (partial transform from-pattern to-pattern))

(defn- apply-stage
  "Apply a `stage` of transformations to a Hiccup `node`.

  Transformations are applied in order. The earliest successful transformation
  takes precedence over any following transformations. Nil punning is used to
  ascertain whether a match is successful."
  [node {:keys [wrapper transformers default]
         :or   {default identity}
         :as   stage}]
  (default (if-let [new-node (->> (map #(% node) transformers)
                                  (remove nil?)
                                  (first))]
             (if wrapper
               (wrapper node new-node)
               new-node)
             node)))

(defn rewrite
  "Process the nodes of some `hiccup` in one or more transformation `stages`.

  Stages are maps with the following keys (optional):
    :transformers - sequence of transformer fns applied to each Hiccup node.
    :wrapper      - fn applied to [node new-node] on successful transformations.
    :default      - fn applied to every Hiccup node as a final step.

  A transformer is an fn that, given a Hiccup node, attempts to match the node,
  returning a transformed node on matches, otherwise returning nil."
  [hiccup & stages]
  (->> (hzip/hiccup-zip hiccup)
       (czip/reduce-zipper (fn [loc node]
                             (if (vector? node)
                               (let [node* (reduce apply-stage node stages)]
                                 (if (not= node node*)
                                   (zip/replace loc node*)
                                   loc))
                               loc)))))

(defn scan
  "Given some `hiccup` and one or more Cuphic `patterns` to match, return a lazy
  sequence of match results. Each result - successful or not - has the form
  [loc bindings-1 ... bindings-n] with n being the number of Cuphic patterns.

  Results come in the order they are scanned while iterating through the zipper.
  Many result rows will probably have nil results for most or all patterns.
  The result rows also include the loc, making it possible to see the exact
  state of the zipper at each node and retrieve the node itself."
  [hiccup & patterns]
  (let [pattern->bindings-fn #(comp (partial get-bindings %) first)
        bindings-fns         (map pattern->bindings-fn patterns)
        loc->result-row      (apply juxt identity bindings-fns)]
    (->> (hzip/hiccup-zip hiccup)
         (czip/iterate-zipper)
         (map loc->result-row))))

(defn select-all
  "Select all nodes in `hiccup` matching the Cuphic `pattern`."
  [hiccup pattern]
  (->> (scan hiccup pattern)
       (remove (comp nil? second))
       (map ffirst)))

(defn select-one
  "Select the first node in `hiccup` matching the Cuphic `pattern`."
  [hiccup pattern]
  (first (select-all hiccup pattern)))

(defn scrape
  "Given some `hiccup` and a map `k->pattern` from keys to Cuphic patterns,
  return the map k->results, where results is a collection of bindings for all
  matches found for the pattern identified by k.

  This can be used to mine Hiccup data (e.g. a webpage) using Cuphic patterns
  to match and bind values from target Hiccup nodes:

    (scrape [:div {}
             [:p {:id \"p\"}
              [:span {:id \"span\"}]]]

            {:x '[?tag {:id \"nada\"}]
             :y '[:span {:id ?id}]
             :z '[?tag {:id ?id} ...?]})

   ... which will return:

     {:y [{?id \"span\"}]
      :z [{?tag :p
           ?id  \"p\"}
          {?tag :span
           ?id  \"span\"}]}

  For a more low-level operation, try the scan function defined above instead."
  [hiccup k->pattern]
  (let [i->k  (into {} (map-indexed vector (keys k->pattern)))
        scans (apply scan hiccup (vals k->pattern))]
    (reduce (fn [m [loc & results]]
              (->> (map-indexed (fn [i v]
                                  (when v
                                    [(i->k i) [(with-meta v {:loc loc})]]))
                                results)
                   (remove nil?)
                   (into {})
                   (merge-with into m)))
            {}
            scans)))
