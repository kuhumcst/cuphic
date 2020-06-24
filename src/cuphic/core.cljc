(ns cuphic.core
  "Data transformations for hiccup."
  (:require [clojure.spec.alpha :as s]
            [clojure.zip :as zip]
            [hickory.zip :as hzip]
            [cuphic.spec :as cs]
            [lambdaisland.deep-diff2 :as dd]
            #?(:cljs [lambdaisland.deep-diff2.diff-impl :refer [Mismatch
                                                                Deletion
                                                                Insertion]]))
  #?(:clj (:import [lambdaisland.deep_diff2.diff_impl Mismatch
                                                      Deletion
                                                      Insertion])))

(defn vector-map-zip
  "Also zips maps in addition to zipping vectors. Intentionally skips records."
  [root]
  (zip/zipper (some-fn vector?                              ; branch?
                       (every-pred map? (complement record?)))
              seq                                           ; children
              (fn [node children]                           ; make-node
                (if (vector? node)
                  (with-meta (into [] children) (meta node))
                  (with-meta (into {} children) (meta node))))
              root))

;; https://groups.google.com/d/msg/clojure/FIJ5Pe-3PFM/JpYDQ2ejBgAJ
(defn skip-subtree
  "Fast-forward a zipper to skip the subtree at `loc`."
  [[node _ :as loc]]
  (cond
    (zip/end? loc) loc
    (zip/right loc) (zip/right loc)
    (zip/up loc) (recur (zip/up loc))
    :else (assoc loc 1 :end)))

(defn- skip-siblings
  "Skip the sibling nodes of the node at `loc`."
  [[node _ :as loc]]
  (or (zip/right loc) (zip/rightmost loc)))

;; TODO: unnecessary? remove?
;; Check if a keyword Insertion is inside a map.
(def ^:private loc-in-map?
  (comp map? zip/node zip/up zip/up))

;; TODO: replace brittle deep-diff2, sometimes different diffs in clj vs. cljs
(defn- attr-bindings
  "Get the symbol->value mapping found when comparing `cattr` to `hattr`.
  Returns nil if the two attrs don't match."
  [cattr hattr]
  (let [diffs (dd/diff cattr hattr)]
    (loop [loc (vector-map-zip diffs)
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
                       (loc-in-map? loc))
              (recur (zip/next loc) ret))

            ;; Mismatches can indicate matching logic variables.
            Mismatch
            (when (symbol? -)
              (if (s/valid? ::cs/? -)
                (recur (zip/next loc) (assoc ret - +))
                (recur (zip/next loc) ret)))

            (recur (zip/next loc) ret)))))))

(defn- hicv
  "Helper function for normalised destructuring of a hiccup-vector `v`."
  [v]
  (if (map? (second v))
    v
    (into [(first v) {}] (rest v))))

(defn- single-or-nil
  [coll]
  (if (not (<= (count coll) 1))
    (throw (ex-info "Too many items in coll (items > 1)" coll))
    (first coll)))

(defn- find-quantifier
  [v]
  (single-or-nil (filter (partial s/valid? ::cs/quantifier) v)))

;; The presence of a quantifier means we're skipping the branch subtree in order
;; to preserve the parallel state of the zippers. The remaining child nodes must
;; still be checked for bindings. This is what coll-bindings accomplishes by
;; calling bindings-delta on every child node not captured by the quantifier.
;; The fragment-bindings special case also requires this for the same reason.
(declare bindings)
(declare bindings-delta)

(defn- coll-bindings
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

(defn- tag+attr-bindings
  "Get the symbol->value mapping found when comparing only tags and attrs of a
  normalised Cuphic vector `cv` and a normalised Hiccup vector `hv`.
  Returns nil if the two vectors don't match."
  [[ctag cattr :as cv] [htag hattr :as hv]]
  (when (= (count cv)
           (count hv))
    (cond
      ;; If the tags match, we can rely on checking attr bindings.
      (= ctag htag)
      (attr-bindings cattr hattr)

      ;; Otherwise, the Cuphic tag can only be a single-value placeholder,
      ;; keeping in mind that quantifiers are dealt with elsewhere.
      (s/valid? ::cs/? ctag)
      (merge
        (when (s/valid? ::cs/? ctag)
          {ctag htag})
        (attr-bindings cattr hattr)))))

(defn- quantifier-ret
  "Helper function for returning the value of quantifier-bindings.
  Ensures that the + quantifier has at least 1 item."
  [quantifier quantified-items & deltas]
  (when (not (and (s/valid? ::cs/+ quantifier)
                  (empty? quantified-items)))
    (apply merge
           (when (not-empty quantified-items)
             {quantifier quantified-items})
           deltas)))

(defn- quantifier-bindings
  "Get the symbol->value mapping found when comparing `ccoll` and `hcoll` when
  `ccoll` contains a `quantifier`. Returns nil if the two vectors don't match.

  Note: quantifiers cannot replace tags as that would break Hiccup semantics.
  You may use [? *] instead to match any Hiccup tag."
  [quantifier ccoll hcoll]
  (cond
    ;; Affixed items
    (= (last ccoll) quantifier)
    (let [ccoll*     (subvec ccoll 0 (dec (count ccoll)))
          split-pos  (count ccoll*)
          hcoll*     (subvec hcoll 0 split-pos)
          coll-delta (coll-bindings ccoll* hcoll*)]
      (when coll-delta
        (quantifier-ret quantifier (subvec hcoll split-pos) coll-delta)))

    ;; Prefixed items
    (= (first ccoll) quantifier)
    (let [ccoll*     (subvec ccoll 1)
          split-pos  (- (count hcoll) (count ccoll*))
          hcoll*     (subvec hcoll split-pos)
          coll-delta (coll-bindings ccoll* hcoll*)]
      (when coll-delta
        (quantifier-ret quantifier (subvec hcoll 0 split-pos) coll-delta)))

    ;; Infixed items
    :else
    (when (>= (count hcoll) (dec (count ccoll)))
      (let [[before [_ & after]] (split-with #(not= quantifier %) ccoll)
            mid          (count before)
            mid-end      (- (count hcoll) (count after))
            before-delta (coll-bindings before (subvec hcoll 0 mid))
            after-delta  (coll-bindings after (subvec hcoll mid-end))]
        (when (and before-delta after-delta)
          (quantifier-ret quantifier
                          (subvec hcoll mid mid-end)
                          before-delta
                          after-delta))))))

(defn- section-search
  "Find the bindings of the first occurrence of fixed-length sequence `ccoll` in
  `nodes` starting the search at the `begin` index. As an aid to the caller, the
  indices of the matching subsection are attached as metadata."
  [ccoll nodes begin]
  (let [section-size (count ccoll)
        end          (- (count nodes) section-size)]
    (loop [i begin]
      (when (<= i end)
        (let [section-end (+ i section-size)
              candidate   (subvec nodes i section-end)]
          (if-let [delta (coll-bindings ccoll candidate)]
            (with-meta delta {:begin i
                              :end   section-end})
            (recur (inc i))))))))

;; TODO: tests
;; TODO: :source
;; TODO: equivalent to-cuphic tranformation using fragments
(defn- fragment-bindings
  "Given a `fragment` Cuphic vector and a sequence of `nodes`, return a sequence
  of bindings for matching sections."
  [fragment nodes & {:keys [limit] :as opts}]
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
      (loop [i      0
             search {}
             ret    []]
        (cond
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
                                  0)
                between-end   (or (:begin (meta (:after search)))
                                  (:begin (meta (:recur search)))
                                  (count nodes))
                between       (subvec nodes between-begin between-end)
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
    (let [cv (hicv cnode)
          hv (hicv hnode)]
      ;; Fragments are a special case with handling similar to the * quantifier.
      ;; If tag and attr match, the children will be scanned for the fragment.
      ;; Note: assumes that `[:<> ...]` has been coerced to `[? [:<> ...]]`.
      (if-let [[cv-n fragment] (->> (map-indexed vector cv)
                                    (filter (comp #(s/valid? ::cs/fragment %)
                                                  second))
                                    (first))]
        ;; TODO: performance improvements, should only do initial 1-fragment search if before-cv contains no quantifiers
        ;; TODO: expand description, relationship with quantifiers
        ;; TODO: support quantifier for after-hv content too
        ;; A fragment is essentially a special quantifier.
        (when-let [hit (-> (fragment-bindings fragment (subvec hv 2) :limit 1)
                           (first))]
          (let [before-cv    (subvec cv 0 cv-n)
                after-cv     (subvec cv (min (inc cv-n) (count cv)))
                hv-n         (+ 2 (:begin (meta hit)))
                fragment-pos (max cv-n hv-n)
                before-hv    (subvec hv 0 fragment-pos)
                ;; TODO: clarify between-end
                between-end  (min (max (- (count hv)
                                          (count after-cv))
                                       0)
                                  (count hv))
                between-hv   (subvec hv fragment-pos between-end)
                after-hv     (subvec hv between-end)]
            ;; TODO: what about matching 0 fragments?
            (when-let [before-delta (bindings before-cv before-hv)]
              (when-let [after-delta (coll-bindings after-cv after-hv)]
                (when-let [<> (-> (fragment-bindings fragment between-hv)
                                  (not-empty))]
                  (with-meta
                    (merge before-delta
                           after-delta
                           {'<> (with-meta
                                  <>
                                  {:begin (+ fragment-pos
                                             (:begin (meta (first <>))))
                                   :end   (+ fragment-pos
                                             (:end (meta (last <>))))})})
                    {:skip [cv hv]}))))))

        ;; TODO: multiple quantifiers should be allowed between non-quantifier segments
        ;; For a regular Cuphic vector:
        ;;   1) Either return a potential quantifier binding + other bindings.
        ;;   2) Otherwise, only return local bindings in tag and attr.
        ;;   3) If the nodes don't match, nil will bubble up and exit the loop.
        (if-let [q (find-quantifier cv)]
          (let [cv*   (subvec cv 0 2)
                hv*   (subvec hv 0 2)
                ccoll (subvec cv 2)
                hcoll (subvec hv 2)]
            (when-let [tag+attr-delta (tag+attr-bindings cv* hv*)]
              (when-let [quantifier-delta (quantifier-bindings q ccoll hcoll)]
                (with-meta (merge tag+attr-delta quantifier-delta)
                           {:skip [cv hv]}))))
          (tag+attr-bindings cv hv))))

    ;; Leafs (= content values) can be captured as bindings here.
    (s/valid? ::cs/? cnode)
    {cnode hnode}))

(defn bindings
  "Get the symbol->value mapping found when comparing `cuphic` to `hiccup`.
  Returns nil if the hiccup does not match the cuphic.

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
        ;;   1) If the cuphic is a fragment, e.g. [:<> ...].
        ;;   2) If the cuphic contains a quantifier, e.g. + or *.
        ;; TODO: analyse delta rather than metadata?
        ;; Currently, metadata notifies the `bindings` function of the need to
        ;; skip subtrees, although this could also be accomplished by looking at
        ;; the delta itself.
        (let [[cloc* hloc*] (if (:skip (meta delta))
                              [(skip-subtree cloc) (skip-subtree hloc)]
                              [cloc hloc])]
          (recur (zip/next cloc*) (zip/next hloc*) (merge ret delta)))))))

(defn matches
  "Returns the match, if any, of `hiccup` to `cuphic`."
  [cuphic hiccup]
  (when (bindings cuphic hiccup)
    hiccup))

(defn- splice-fragments
  "Given a `fragment-loc` and a sequence of `fragment-bindings`, replace the
  loc with multiple fragments with individual bindings applied."
  [[node :as fragment-loc] fragment-bindings]
  (let [fragment-parts  (subvec (hicv node) 2)
        splice-fragment (fn [loc symbol->value]
                          (->> (map #(get symbol->value % %) fragment-parts)
                               (reduce zip/insert-left loc)))]
    (zip/remove (reduce splice-fragment fragment-loc fragment-bindings))))

;; TODO: need splicing for quantifiers too
(defn apply-bindings
  "Apply `symbol->value` bindings to a piece of `cuphic`."
  [symbol->value cuphic]
  (loop [[node :as loc] (vector-map-zip cuphic)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (cond
                         (symbol->value node)
                         (zip/replace loc (symbol->value node))

                         (and (contains? symbol->value '<>)
                              (vector? node)
                              (= :<> (first node)))
                         (splice-fragments loc (get symbol->value '<>))

                         :else loc))))))

(defn transform
  "Transform hiccup using cuphic from/to templates.

  Substitutes symbols in `to` with bound values from `hiccup` based on symbols
  in `from`. The cuphic templates can also be replaced with functions that
  either produce or consume a symbol->value map. "
  [from to hiccup]
  (when-let [symbol->value (if (fn? from)
                             (from hiccup)
                             (bindings from hiccup))]
    (if (fn? to)
      (to symbol->value)
      (apply-bindings symbol->value to))))

(defn transformer
  "Make a transform fn to transform hiccup using cuphic from/to templates."
  [& {:keys [from to]}]
  (partial transform from to))

(defn- apply-stage
  "Apply a `stage` of transformations to a Hiccup `node`."
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
  "Process the nodes of `hiccup` tree in a sequence of `stages`.

  Stages are maps with the following keys (optional):
    :transformers - sequence of transformer fns applied to each Hiccup node.
    :wrapper      - fn applied to [node new-node] on successful transformations.
    :default      - fn applied to every Hiccup node as a final step.

  Note: a transformer is an fn that, given a Hiccup node, attempts to match the
  node, returning a transformed node on matches, otherwise returning nil."
  [hiccup stages]
  (loop [[node :as loc] (hzip/hiccup-zip hiccup)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (if (vector? node)
                         (let [new-node (reduce apply-stage node stages)]
                           (if (not= node new-node)
                             (zip/replace loc new-node)
                             loc))
                         loc))))))
