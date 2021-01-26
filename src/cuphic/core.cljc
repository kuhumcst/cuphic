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
            [cuphic.zip :as czip])
  #?(:clj (:import [lambdaisland.deep_diff2.diff_impl Mismatch
                                                      Deletion
                                                      Insertion])))

(def quantifier?
  (partial s/valid? ::cs/quantifier))

(def fragment?
  (partial s/valid? ::cs/fragment))

(def not-fragment?
  (complement fragment?))

(defn- hicv
  "Helper function for normalised destructuring of a Hiccup vector `v`."
  [v]
  (if (map? (second v))
    v
    (into [(first v) {}] (rest v))))

(defn- ->nodes
  [v]
  (if (map? (second v))
    (subvec v 2)
    (subvec v 1)))

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

(defn- tag+attr-bindings
  "Get the symbol->value mapping found when comparing only tags and attrs of a
  normalised Cuphic vector `cv` and a normalised Hiccup vector `hv`.
  Returns nil if the two vectors don't match."
  [[ctag cattr] [htag hattr]]
  (cond
    ;; If the tags match, we can rely on checking attr bindings.
    (= ctag htag)
    (attr-bindings cattr hattr)

    ;; Otherwise, the Cuphic tag can only be a single-value placeholder,
    ;; keeping in mind that quantifiers are dealt with elsewhere.
    (s/valid? ::cs/? ctag)
    (when-let [bindings (attr-bindings cattr hattr)]
      (merge {ctag htag} bindings))))

;; Some binding helper fns require recursive calls to the parent fns.
(declare bindings-delta)

(defn- fixed-bindings
  "Get the symbol->value mapping found comparing two sequential collections.
  This function is used to scoop up bindings that would otherwise be skipped
  if a quantifier is present.
  Returns nil if the two vectors don't match."
  [ccoll hcoll]
  (when (= (count ccoll) (count hcoll))                     ; performance optm.
    (loop [[cnode & ccoll] ccoll
           [hnode & hcoll] hcoll
           ret {}]
      (if (and cnode hnode)
        (when-let [delta (bindings-delta cnode hnode)]
          (recur ccoll hcoll (merge ret delta)))
        (when (and (empty? ccoll) (empty? hcoll))
          ret)))))

(defn- section-search
  "Find the bindings of the first occurrence of fixed-length sequence `cnodes`
  in `hnodes` starting the search at the `begin` index. As an aid to the caller,
  the indices of the matching subsection are attached as metadata."
  [cnodes hnodes begin]
  (when (not (empty? cnodes))
    (let [section-size (count cnodes)
          end          (- (count hnodes) section-size)]
      (loop [i begin]
        (when (<= i end)
          (let [section-end (+ i section-size)
                candidate   (subvec hnodes i section-end)]
            (if-let [delta (fixed-bindings cnodes candidate)]
              (with-meta delta {:begin i
                                :end   section-end})
              (recur (inc i)))))))))

(defn- direct-bindings
  "Return 1:1 bindings between `pattern` and `nodes` as kvs."
  [pattern nodes]
  (when (= (count pattern) (count nodes))
    (reduce (fn [kvs [k v :as kv]]
              (cond
                (= k v) kvs
                (symbol? k) (conj kvs kv)
                (vector? v) (apply conj kvs (bindings-delta k v))
                :else (reduced nil)))
            []
            (map vector pattern nodes))))

(defn capture-pattern
  "Create a capture pattern from a sequence of `cnodes`. Returns the input data
  structure with relevant metadata attached, or nil if invalid."
  [cnodes]
  (let [parts            (partition-by quantifier? cnodes)
        quantifiers      (filter (comp quantifier? first) parts)
        quantifier-count (count quantifiers)
        min-count        (fn []
                           (if (s/valid? ::cs/+ (ffirst quantifiers))
                             (count cnodes)
                             (dec (count cnodes))))]
    (cond
      (> quantifier-count 1)
      nil

      (= quantifier-count 0)
      (with-meta cnodes {:min-count (count cnodes)})

      (quantifier? (first cnodes))
      (with-meta cnodes {:quantifier :prefix
                         :parts      parts
                         :min-count  (min-count)})

      (quantifier? (last cnodes))
      (with-meta cnodes {:quantifier :affix
                         :parts      parts
                         :min-count  (min-count)})

      :else
      (with-meta cnodes {:quantifier :infix
                         :parts      parts
                         :min-count  (min-count)}))))

(defn ->patterns
  "Partition a list of `cnodes` into a list of capture patterns."
  [cnodes]
  (->> (partition-by symbol? cnodes)
       (map capture-pattern)))

(defn- min-capture
  "Return the aggregate min-count of a list of `capture-patterns`."
  [capture-patterns]
  (reduce + (map (comp :min-count meta) capture-patterns)))

(defn- pattern-bindings
  "Given a list of cnodes partitioned into `capture-patterns`, return the
  bindings found in `hnodes`."
  [capture-patterns hnodes]
  (loop [ret    {}
         hnodes hnodes
         [pattern & [next-pattern :as patterns]] capture-patterns]
    (let [{:keys [quantifier parts min-count]} (meta pattern)
          nodes-until (fn [cnodes hnodes begin]
                        (let [hit (section-search cnodes hnodes begin)]
                          (subvec hnodes 0 (or (:begin (meta hit))
                                               (count hnodes)))))
          min-nodes?  (fn [quantifier nodes]
                        (not (and (= 0 (count nodes))
                                  (s/valid? ::cs/+ quantifier))))]
      (if pattern
        (when (<= min-count (count hnodes))
          (case quantifier
            ;; With no quantifier, we simply capture the direct bindings.
            nil (let [bound   (subvec hnodes 0 min-count)
                      unbound (subvec hnodes min-count)
                      ret*    (into ret (direct-bindings pattern bound))]
                  (recur ret* unbound patterns))

            :prefix (let [[[quantifier] after] parts
                          bound   (nodes-until next-pattern hnodes min-count)
                          unbound (subvec hnodes (count bound))
                          split   (- (count bound) (count after))
                          qnodes  (subvec bound 0 split)
                          anodes  (subvec bound split)
                          kvs     (concat (direct-bindings after anodes)
                                          {quantifier qnodes})]
                      (when (min-nodes? quantifier qnodes)
                        (recur (apply merge ret kvs) unbound patterns)))

            :affix (let [[before [quantifier]] parts
                         bound   (nodes-until next-pattern hnodes min-count)
                         unbound (subvec hnodes (count bound))
                         split   (count before)
                         bnodes  (subvec bound 0 split)
                         qnodes  (subvec bound split)
                         kvs     (concat (direct-bindings before bnodes)
                                         {quantifier qnodes})]
                     (when (min-nodes? quantifier qnodes)
                       (recur (apply merge ret kvs) unbound patterns)))

            :infix (let [[before [quantifier] after] parts
                         bound   (nodes-until next-pattern hnodes min-count)
                         unbound (subvec hnodes (count bound))
                         split-1 (count before)
                         split-2 (- (count bound) (count after))
                         bnodes  (subvec bound 0 split-1)
                         anodes  (subvec bound split-2)
                         qnodes  (subvec bound split-1 split-2)
                         kvs     (concat (direct-bindings before bnodes)
                                         (direct-bindings after anodes)
                                         {quantifier qnodes})]
                     (when (min-nodes? quantifier qnodes)
                       (recur (apply merge ret kvs) unbound patterns)))))
        ret))))

;; TODO: tests
;; TODO: :source
(defn- fragment-bindings
  "Given a `fragment` Cuphic vector and a sequence of `nodes`, return a sequence
  of bindings for matching sections."
  [fragment nodes & {:keys [limit begin end] :as opts}]
  (let [!quantifier (complement (partial s/valid? ::cs/quantifier))
        ccoll       (if (map? (second fragment))
                      (subvec fragment 2)
                      (subvec fragment 1))
        [before [quantifier & after]] (split-with !quantifier ccoll)
        nodes-size  (count nodes)
        min-size    (+ (count before)
                       (count after)
                       (if (s/valid? ::cs/+ quantifier) 1 0))]
    (when (>= (count nodes) min-size)                       ; For performance
      (loop [i      (or begin 0)
             search {}
             ret    []]
        (cond
          ;; Optional early return when a set end has been reached.
          (> i end)
          ret

          ;; Optional early return when a result limit has been set.
          (= (count ret) limit)
          ret

          ;; Return result vector when the nodes are exhausted.
          (and (>= i nodes-size)
               (empty? search))
          ret

          ;; Search for the `before` section when applicable.
          ;; Returns the result vector if the search is inconclusive.
          (and (not (:before search))
               (seq before))
          (if-let [before-search (section-search before nodes i)]
            (recur (:end (meta before-search))
                   (assoc search :before before-search)
                   ret)
            ret)

          ;; Search for the `after` section when applicable.
          ;; Returns the result vector if the search is inconclusive.
          (and (not (:after search))
               (seq after))
          (if-let [after-search (section-search after nodes i)]
            (recur (:end (meta after-search))
                   (assoc search :after after-search)
                   ret)
            ret)

          ;; Search for a recurrence of the `before` section to avoid greedy
          ;; incidental matching of any upcoming fragments.
          (and (not (:recur search))
               (seq before)
               (not (seq after)))
          (let [recur-search (section-search before nodes i)]
            (recur (or (:begin (meta recur-search))
                       i)
                   (assoc search :recur (or recur-search {}))
                   ret))

          ;; Capture nodes from `before` to `after` sections when applicable.
          ;; If there is no `after` section, the quantifier either:
          ;;   1) Captures until the next recurrence of the `begin` section.
          ;;   2) Captures all of the remaining nodes.
          quantifier
          (let [between-begin (or (:end (meta (:before search)))
                                  (:end (meta (last ret)))
                                  begin)
                between-end   (or (:begin (meta (:after search)))
                                  (:begin (meta (:recur search)))
                                  (count nodes))
                between       (if (< between-begin between-end)
                                (subvec nodes between-begin between-end)
                                [])
                ret*          (when (not (and (empty? between)
                                              (s/valid? ::cs/+ quantifier)))
                                (let [section {:begin (- between-begin
                                                         (count before))
                                               :end   (+ between-end
                                                         (count after))}]
                                  (conj ret (with-meta
                                              (merge {quantifier between}
                                                     (:before search)
                                                     (:after search))
                                              section))))]
            (recur (max i between-end) {} (or ret* ret)))

          ;; If there is NO quantifier, i.e. the entire fragment is `before`,
          ;; we add the bindings to the result vector and increment the index.
          (:before search)
          (let [before-section (meta (:before search))
                section        {:begin (:begin before-section)
                                :end   (+ (:begin before-section)
                                          (count before))}
                ret*           (conj ret (with-meta (merge (:before search)
                                                           (:after search))
                                                    section))]
            (recur (:end before-section) {} ret*))

          ;; An unsuccessful search will return the empty list of results.
          :else ret)))))

(defn- bindings-delta
  "Get a delta of the local bindings as a map by comparing `cnode` to `hnode`.
  Will return nil if the two nodes do not match."
  [cnode hnode]
  (cond
    ;; Nothing to bind. Skip to next node.
    (= cnode hnode)
    {}

    ;; Branches (= Hiccup vectors) can be captured here.
    (and (vector? cnode)
         (vector? hnode))
    (let [cv     (hicv cnode)
          hv     (hicv hnode)
          cnodes (subvec cv 2)
          hnodes (subvec hv 2)]
      (when-let [tag+attr-delta (tag+attr-bindings (subvec cv 0 2)
                                                   (subvec hv 0 2))]
        ;; Fragments are bounded by segments on either side.
        (let [[before [fragment & after]] (split-with not-fragment? cnodes)]
          (if fragment
            (let [bpatterns      (not-empty (->patterns before))
                  apatterns      (not-empty (->patterns after))
                  fnodes         (->nodes fragment)
                  fragment-count (min-capture (->patterns fnodes))
                  before-count   (min-capture bpatterns)
                  after-count    (min-capture apatterns)
                  max-count      (count hnodes)]
              ;; As a performance optimisation, only proceed if hnodes fits the
              ;; min-length requirements.
              (when (<= (+ before-count after-count fragment-count)
                        max-count)
                ;; Fragment search is greedy within the bounded context.
                (let [<> (fragment-bindings fragment hnodes
                                            :begin before-count
                                            :end (- max-count after-count))]
                  (when (not-empty <>)
                    (let [split-1 (:begin (meta (first <>)))
                          split-2 (:end (meta (last <>)))
                          bnodes  (subvec hnodes 0 split-1)
                          anodes  (subvec hnodes split-2)]
                      (when-let [bdelta (if bpatterns
                                          (pattern-bindings bpatterns bnodes)
                                          {})]
                        (when-let [adelta (if apatterns
                                            (pattern-bindings apatterns anodes)
                                            {})]
                          (let [qdelta {'<> (with-meta <>
                                                       {:begin (+ 2 split-1)
                                                        :end   (+ 2 split-2)})}]
                            (with-meta (merge tag+attr-delta
                                              bdelta
                                              adelta
                                              qdelta)
                                       {:skip [cv hv]})))))))))

            (when-let [delta (pattern-bindings (->patterns cnodes) hnodes)]
              (with-meta (merge tag+attr-delta delta)
                         {:skip [cv hv]}))))))

    ;; Leafs (= content values) can be captured as bindings here.
    (s/valid? ::cs/? cnode)
    {cnode hnode}))

(defn get-bindings
  "Get the symbol->value mapping found when comparing `cuphic` to `hiccup`.
  Returns nil if the Hiccup does not match the Cuphic.

  The two data structures are zipped through in parallel while their bindings
  are collected incrementally."
  [cuphic hiccup]
  (assert (s/valid? ::cs/cuphic cuphic))                    ; elide in prod
  (loop [cloc (hzip/hiccup-zip cuphic)
         hloc (hzip/hiccup-zip hiccup)
         ret  {}]
    (if (zip/end? hloc)
      (with-meta (dissoc ret '? '* '+) {:source hiccup})
      (when-let [delta (bindings-delta (zip/node cloc) (zip/node hloc))]
        ;; Subtrees should be skipped in two cases:
        ;;   1) If the Cuphic is a fragment, e.g. [:<> ...].
        ;;   2) If the Cuphic contains a quantifier, e.g. + or *.
        ;; TODO: analyse delta rather than metadata?
        ;; Currently, metadata notifies the `bindings` function of the need to
        ;; skip subtrees, although this could also be accomplished by looking at
        ;; the delta itself.
        (let [[cloc* hloc*] (if (:skip (meta delta))
                              [(czip/skip-subtree cloc)
                               (czip/skip-subtree hloc)]
                              [cloc hloc])]
          (recur (zip/next cloc*) (zip/next hloc*) (merge ret delta)))))))

(defn matches
  "Returns the match, if any, of `hiccup` to `cuphic`."
  [cuphic hiccup]
  (when (get-bindings cuphic hiccup)
    hiccup))

(defn- fragment-replace
  "Given a fragment `loc` and one or more fragment `fbindings`, substitute the
  loc with the fragments, each fragment's individual bindings applied."
  [[node :as loc] fbindings]
  (let [parts              (subvec (hicv node) 2)
        fbindings->section (fn [symbol->value]
                             (mapcat (fn [x]
                                       (if (s/valid? ::cs/quantifier x)
                                         (symbol->value x)
                                         [(symbol->value x x)])) parts))
        replacements       (mapcat fbindings->section fbindings)]
    (czip/multi-replace loc replacements)))

(defn apply-bindings
  "Apply symbol->value `bindings` to a piece of `cuphic`."
  [bindings cuphic]
  (loop [[node :as loc] (czip/vector-map-zip cuphic)]
    (if (zip/end? loc)
      (zip/root loc)
      (let [replacement (get bindings node)]
        (recur (zip/next (cond
                           replacement
                           (if (s/valid? ::cs/quantifier node)
                             (czip/multi-replace loc replacement)
                             (zip/replace loc replacement))

                           (and (s/valid? ::cs/fragment node)
                                (contains? bindings '<>))
                           (fragment-replace loc (get bindings '<>))

                           :else loc)))))))

(defn transform
  "Transform `hiccup` using `from` and `to` templates Cuphic templates (or fns).

  Substitutes symbols in `to` with bound values from `hiccup` based on symbols
  in `from`. The Cuphic templates can also be replaced with functions that
  either produce or consume a symbol->value map. "
  [from to hiccup]
  (when-let [symbol->value (if (fn? from)
                             (from hiccup)
                             (get-bindings from hiccup))]
    (if (fn? to)
      (to symbol->value)
      (apply-bindings symbol->value to))))

(defn ->transformer
  "Make a transformer fn to transform Hiccup using Cuphic `from`/`to` templates.

  The returned fn will return the transformed value on successful matches and
  nil otherwise."
  [from to]
  (partial transform from to))

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
  (loop [[node :as loc] (hzip/hiccup-zip hiccup)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (if (vector? node)
                         (let [new-node (reduce apply-stage node stages)]
                           (if (not= node new-node)
                             (zip/replace loc new-node)
                             loc))
                         loc))))))

;; TODO: make a simple select fn too, replacing the rescope one
(defn scan
  "Given some `hiccup` and one or more `cuphic` expressions to match, return a
  lazy sequence of matches, each match having the form [cuphic bindings loc].

  This returns matches in the order they are found while iterating through the
  zipper. It also returns the loc, making it possible to see the exact state of
  the zipper and location of the node. Unless you specifically need access the
  locs directly, just use the scrape fn defined below instead."
  [hiccup & cuphic]
  (->> (hzip/hiccup-zip hiccup)
       (czip/iterate-zipper)
       (mapcat (fn [[node :as loc]]
                 (for [cuphic cuphic
                       :let [bindings (get-bindings cuphic node)]
                       :when bindings]
                   [cuphic bindings loc])))
       (concat)))

(defn scrape
  "Given some `hiccup` and one or more `cuphic` expressions to match, return
  [bindings-1 ... bindings-n] with n being the number of Cuphic expressions.

  This can be used to mine Hiccup data (e.g. a webpage) using Cuphic expressions
  to match and bind values from specific Hiccup nodes:

    (scrape [:div {}
             [:p {:id \"p\"}
              [:span {:id \"span\"}]]]

            '[?tag {:id \"nada\"}]   ; x
            '[:span {:id ?id}]       ; y
            '[?tag {:id ?id}])       ; z

   ... which will return:

     [nil                            ; x -- nothing matches
      ({?id \"span\"})               ; y -- only :span matches
      ({?tag :p, ?id \"p\"}          ; z -- :p and :span both match
       {?tag :span, ?id \"span\"})]

  For a more low-level operation where node locations are needed, use the
  scan fn defined above instead."
  [hiccup & cuphic]
  (let [scans           (apply scan hiccup cuphic)
        cuphic->results (into {} (for [[k v] (group-by first scans)]
                                   [k (map second v)]))]
    (mapv cuphic->results cuphic)))


(comment
  (scan [:p {} [:date {:when "glen"}]]
        '[:nada {:when "does not exist"}]
        '[? {:when "glen"}]
        '[? {:when ?date}])

  (scrape [:div {}
           [:p {:id "p"}
            [:span {:id "span"}]]]

          '[?tag {:id "nada"}]                              ; x
          '[:span {:id ?id}]                                ; y
          '[?tag {:id ?id}])                                ; z

  (let [[b1 b2 b3] (scrape [:p {} [:date {:when "glen"}]]
                           '[? {:when "does not exist"}]
                           '[? {:when "glen"}]
                           '[? {:when ?date}])]
    (zipmap '[b1 b2 b3] [b1 b2 b3]))
  #_.)
