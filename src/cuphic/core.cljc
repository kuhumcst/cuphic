(ns cuphic.core
  "Data transformations for hiccup."
  (:require [clojure.spec.alpha :as s]
            [clojure.zip :as zip]
            [clojure.set :as set]
            [hickory.zip :as hzip]
            [cuphic.util :as util]
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
  (->> v
       (filter (partial s/valid? ::cs/quantifier))
       (single-or-nil)))

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
  "Get the symbol->value mapping found when comparing a normalised Cuphic vector
  `cv` and a normalised Hiccup vector `hv` when `cv` contains a `quantifier`.
  Returns nil if the two vectors don't match.

  Note: quantifiers cannot replace tags as that would break Hiccup semantics.
  You may use [? *] instead to match any Hiccup tag."
  [quantifier cv hv]
  (cond
    ;; TODO: spec check here? somewhere else?
    (= (first cv) quantifier)
    (throw (ex-info "tag cannot be a quantifier" cv))

    ;; Affixed items
    (= (last cv) quantifier)
    (let [cv*            (subvec cv 0 (dec (count cv)))
          hv*            (subvec hv 0 (count cv*))
          tag+attr-delta (tag+attr-bindings cv* hv*)
          coll-delta     (coll-bindings (subvec cv* 2) (subvec hv* 2))]
      (when (and tag+attr-delta coll-delta)
        (let [quantified-items (subvec hv (min (count cv*)) (count hv))]
          (quantifier-ret quantifier
                          quantified-items
                          tag+attr-delta
                          coll-delta))))

    ;; Prefixed items
    (= (get cv 2) quantifier)
    (let [tag+attr-delta (tag+attr-bindings (subvec cv 0 2) (subvec hv 0 2))
          ccoll          (subvec cv 3)
          hcoll          (subvec hv (- (count hv) (count ccoll)))
          coll-delta     (coll-bindings ccoll hcoll)]
      (when (and tag+attr-delta coll-delta)
        (let [quantified-items (subvec hv 2 (- (count hv) (count ccoll)))]
          (quantifier-ret quantifier
                          quantified-items
                          tag+attr-delta
                          coll-delta))))

    ;; Infixed items
    :else
    (when (>= (count hv) (dec (count cv)))
      (let [tag+attr-delta (tag+attr-bindings (subvec cv 0 2) (subvec hv 0 2))
            [before after] (split-with (partial not= quantifier) (subvec cv 2))
            mid            (+ (count before) 2)
            mid-end        (- (count hv) (count (rest after)))
            before-delta   (coll-bindings before
                                          (subvec hv 2 mid))
            after-delta    (coll-bindings (rest after)
                                          (subvec hv mid-end))]
        (when (and tag+attr-delta before-delta after-delta)
          (let [quantified-items (subvec hv mid mid-end)]
            (quantifier-ret quantifier
                            quantified-items
                            tag+attr-delta
                            before-delta
                            after-delta)))))))

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
            (with-meta delta {:section {:begin i
                                        :end   section-end}})
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
            (recur (:end (:section (meta before-search)))
                   (assoc search :before before-search)
                   ret)
            ret)

          ;; Search for the `after` section when applicable.
          ;; Returns the result vector if the search is inconclusive.
          (and (not (:after search))
               (seq after))
          (if-let [after-search (section-search after nodes i)]
            (recur (:end (:section (meta after-search)))
                   (assoc search :after after-search)
                   ret)
            ret)

          ;; Search for a recurrence of the `before` section to avoid greedy
          ;; incidental matching of any upcoming fragments.
          (and (not (:recur search))
               (seq before)
               (not (seq after)))
          (let [recur-search (section-search before nodes i)]
            (recur (or (:begin (:section (meta recur-search)))
                       i)
                   (assoc search :recur (or recur-search {}))
                   ret))

          ;; Capture nodes from `before` to `after` sections when applicable.
          ;; If there is no `after` section, the quantifier either:
          ;;   1) Captures until the next recurrence of the `begin` section.
          ;;   2) Captures all of the remaining nodes.
          quantifier
          (let [between-begin (or (:end (:section (meta (:before search))))
                                  (:end (:section (meta (last ret))))
                                  0)
                between-end   (or (:begin (:section (meta (:after search))))
                                  (:begin (:section (meta (:recur search))))
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
                                              {:section section}))))]
            (recur (max i between-end) {} (or ret* ret)))

          ;; If there is NO quantifier, i.e. the entire fragment is `before`,
          ;; we add the bindings to the result vector and increment the index.
          (:before search)
          (let [before-section (:section (meta (:before search)))
                section        {:begin (:begin before-section)
                                :end   (+ (:begin before-section)
                                          (count before))}
                ret*           (conj ret (with-meta (merge (:before search)
                                                           (:after search))
                                                    {:section section}))]
            (recur (:end before-section) {} ret*))

          ;; An unsuccessful search will return the empty list of results.
          :else ret)))))

(comment
  ;; Fragments are returned as the symbol <> (not :<>) to make destructuring
  ;; easy. The parent structure (and other higher-level substructures) can still
  ;; capture variables. Only a single fragment is allowed for now.
  {<>          [{+stuff [1 2 3]
                 ?other "other"}]
   ?parent-tag :p}
  (section-search '[1 ?glen] [1 2 1 3 3 3 4 3 4 1 5] 2)
  (meta (section-search '[1 ?glen] [1 2 1 3 3 3 4 3 4 1 5] 0))
  (fragment-bindings '[:<> 1 ?num] [1 2 1 3 3 3 4 3 4 1 5])
  (bindings '[? [:<> 1 ?num]] [:p 1 2 1 3 3 3 4 3 4 1 5])

  (bindings '[? [:<> ?num 3]] [:p 1 2 1 3 3 3 4 3 4 1 5])
  (bindings '[? [:<> 1 *any 3]] [:p 1 2 1 3 3 3 4 3 4 1 5])
  (bindings '[? [:<> +any 3]] [:p 1 2 1 3 3 3 4 3 4 1 5])

  #_.)

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
      (if-let [[n fragment] (->> (map-indexed vector cv)
                                 (filter (comp (partial s/valid? ::cs/fragment)
                                               second))
                                 (first))]
        ;; TODO: performance improvements, should only do initial 1-fragment search if before-cv contains no quantifiers
        ;; TODO: expand description, relationship with quantifiers
        ;; TODO: support quantifier for after-hv content too
        ;; A fragment is essentially a special quantifier.
        (when-let [hit (-> (fragment-bindings fragment (subvec hv 2) :limit 1)
                           (first))]
          (let [before-cv   (subvec cv 0 (min n (count cv)))
                before-hv   (subvec hv 0 (+ 2 (:begin (:section (meta hit)))))
                after-cv    (subvec cv (min (inc n) (count cv)))
                between-end (min (max (- (count hv)
                                         (count after-cv))
                                      0)
                                 (count hv))
                after-hv    (subvec hv between-end)
                between-hv  (subvec hv n between-end)]
            ;; TODO: what about matching 0 fragments?
            (when-let [before-delta (bindings before-cv before-hv)]
              (when-let [after-delta (coll-bindings after-cv after-hv)]
                (when-let [<> (-> (fragment-bindings fragment between-hv)
                                  (not-empty))]
                  (with-meta (merge before-delta
                                    after-delta
                                    {'<> <>})
                             {:skip [cv hv]}))))))

        ;; For a regular Cuphic vector:
        ;;   1) Either return a potential quantifier binding + other bindings.
        ;;   2) Otherwise, only return local bindings in tag and attr.
        ;;   3) When the nodes don't match, nil will bubble up and exit the loop.
        (if-let [quantifier (find-quantifier cv)]
          (when-let [symbol->value (quantifier-bindings quantifier cv hv)]
            (with-meta symbol->value {:skip [cv hv]}))
          (tag+attr-bindings cv hv))))

    ;; Leafs (= content values) can be captured as bindings here.
    (s/valid? ::cs/? cnode)
    {cnode hnode}))


(comment
  ;; The first two numbers are captured as ?x and ?y, the rest in <>
  (bindings '[?tag ?x ?y [:<> 1 2]]
            [:p 1 2 1 2])
  ;; The same applies to the numbers after here
  (bindings '[?tag ?x [:<> 2 1] ?y]
            [:p 0 2 1 3])

  ;; Behaviour of quantifiers in fragments demonstrating how capturing ends
  ;; when a pattern recurs.
  (bindings '[?tag ?x [:<> ?n *rest] ?y]                    ; *rest is empty
            [:p 0 1 2 3 1 2 3 99])
  (bindings '[?tag ?x [:<> ?n +rest] ?y]                    ; no match as +rest conflicts with ?n in next fragment
            [:p 0 1 2 3 1 2 3 99])
  (bindings '[?tag ?x [:<> ?a ?b] ?y]                       ; every pair matches
            [:p 0 1 2 3 4 5 6 99])

  ;; Relationship between quantifiers and fragments.
  (-> (bindings '[:div {:type ?type}
                  +before
                  [:<>
                   [:pb {:n ?n :facs ?facs}]
                   +page-content]]
                [:div {:type "letter"}

                 ;; +before
                 [:fw {}]
                 [:opener
                  {:xml:id "xx"}
                  [:dateline {} [:settlement {:ref "#STED"}] [:date {:when "1942-06-30"}]]
                  [:salute {} [:persname {:type "receiver", :ref "xx"} "MODTAGER "]]]

                 ;; <>, first
                 [:pb {:n "1", :facs "24.628"}]
                 [:p {:xml:id "p1"} [:persname {:ref "#np60"} "Jens Holt"] " , 16 "]
                 [:p {:xml:id "p2"} [:date {:when "1942-06-30"} "30 juni 1942"]]
                 [:p {:xml:id "p3"} "Kære " [:persname {:ref "#np60"} "Holt"] " ,"]

                 ;; <>, second
                 [:pb {:n "2", :facs "24.629"}]
                 [:p {:xml:id "p5"} "2 "]
                 [:p {:xml:id "p6"} "mit bord maaned efter maaned et fuldt færdigt manuskript , som ikke kan komme ud !"]])
      #_(keys))


  #_.)

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

(comment
  ;; Should fail as [:div ...] does not directly contain any [:<> 1 2].
  (transform '[?tag [:<> 1 2]]
             '[?tag [:<> 3 4]]
             [:div
              [:head
               [:p 1 2 1 2]]])

  ;; Switches places of the numbers.
  (transform '[?tag [:<> ?x ?y]]
             '[?tag [:<> ?y ?x]]
             [:p 4 5 1 2 1 2])

  ;; TODO: silently throws away first two numbers? Is this desirable?
  (transform '[?tag [:<> 1 2]]
             '[?tag [:<> 99 88]]
             [:p 4 5 1 2 1 2])
  #_.)

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

(defn- as-data-*
  [attr]
  (into {} (for [[k v] attr]
             [(keyword (util/data-* k)) v])))

(defn- attr->data-attr
  "Convert all attributes into data-* attributes."
  [[[tag attr & content :as node] _ :as loc]]
  (if (map? attr)
    (zip/edit loc assoc 1 (as-data-* attr))
    loc))

(defn- rename-attr
  "Rename attr keys according to `kmap`."
  [kmap [[tag attr & content :as node] _ :as loc]]
  (if (and kmap (map? attr))
    (zip/edit loc assoc 1 (set/rename-keys attr (as-data-* kmap)))
    loc))

;; Only modifies metadata. Later this is merged into attr by meta-into-attr.
(defn- inject
  "Insert transformed Hiccup when node `loc` matches one of the `transformers`.
  A `wrapper` fn taking [old-node new-node] as args can be supplied to modify
  the new node and its metadata. If no wrapper is supplied, the new node
  entirely replaces the old node in the tree."
  [wrapper transformers [[tag attr & content :as node] _ :as loc]]
  (if-let [hiccup (->> (map #(% node) transformers)
                       (remove nil?)
                       (first))]
    (let [new-node (if wrapper
                     (wrapper node hiccup)
                     (vary-meta hiccup assoc :replaced? true))]
      (zip/replace loc new-node))
    loc))

(defn- add-prefix
  "Transform a hiccup vector node `loc` to a valid custom element name by
  setting a custom `prefix`."
  [prefix [node _ :as loc]]
  (let [tag     (name (first node))
        new-tag (keyword (util/prefixed prefix tag))]
    (zip/edit loc assoc 0 new-tag)))

(defn- meta-into-attr
  "Merge the element metadata into the attr. Mimics the behaviour of reagent."
  [[[tag attr & content :as node] _ :as loc]]
  (if-let [m (meta node)]
    (zip/edit loc update 1 merge m)
    loc))

;; Every branch is a hiccup vector, while everything else is a leaf.
(defn- edit-branch
  [loc {:keys [prefix attr-kmap wrapper transformers]
        :as   opts}]
  (let [loc*        (inject wrapper transformers loc)
        ?add-prefix (fn [loc] (if (not-empty prefix)
                                (add-prefix prefix loc)
                                loc))]
    (if (:replaced? (meta (zip/node loc*)))
      (skip-siblings loc*)
      (->> loc*
           (attr->data-attr)
           (rename-attr attr-kmap)
           (?add-prefix)
           (meta-into-attr)))))

(defn rewrite
  "Process relevant nodes of a zipper made from a `hiccup` tree based on `opts`.
  Return the transformed structure."
  [hiccup {:keys [prefix attr-kmap wrapper transformers] :as opts}]
  (loop [[node :as loc] (hzip/hiccup-zip hiccup)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (if (vector? node)
                         (edit-branch loc opts)
                         loc))))))
