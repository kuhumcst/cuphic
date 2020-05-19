(ns cuphic.core
  "Data transformations for hiccup."
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clojure.string :as str]
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
  "Also zips maps in addition to zipping vectors."
  [root]
  (zip/zipper (some-fn vector? (every-pred map? (complement record?)))
              seq
              (fn [node children] (with-meta (vec children) (meta node)))
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

(defn- bindings-delta
  "Get a delta of the local bindings as a map by comparing `cnode` to `hnode`.
  Will return nil if the two nodes do not match."
  [cnode hnode]
  (cond
    ;; Nothing to bind. Skip to next node.
    (= cnode hnode)
    {}

    ;; For a zipper branch (= Hiccup vector):
    ;;   1) Either return a potential quantifier binding (with other bindings).
    ;;   2) Otherwise, only return local bindings in tag and attr.
    ;;   3) When the nodes don't match, nil will bubble up and exit the loop.
    (and (vector? cnode)
         (vector? hnode))
    (let [cv (hicv cnode)
          hv (hicv hnode)]
      (if-let [quantifier (find-quantifier cv)]
        (when-let [symbol->value (quantifier-bindings quantifier cv hv)]
          (with-meta symbol->value {:quantifier? true}))
        (tag+attr-bindings cv hv)))

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
        ;; Quantifiers mark vectors of dissimilar length, so in order for the
        ;; two zippers to stay in sync their subtrees must be skipped.
        (let [[cloc* hloc*] (if (:quantifier? (meta delta))
                              [(skip-subtree cloc) (skip-subtree hloc)]
                              [cloc hloc])]
          (recur (zip/next cloc*) (zip/next hloc*) (merge ret delta)))))))

(defn matches
  "Returns the match, if any, of `hiccup` to `cuphic`."
  [cuphic hiccup]
  (when (bindings cuphic hiccup)
    hiccup))

(defn apply-bindings
  "Apply `symbol->value` bindings to a piece of `cuphic`."
  [symbol->value cuphic]
  (walk/postwalk #(get symbol->value % %) cuphic))

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

(defn- trim-str
  "Remove any blank strings from a string node `loc`."
  [[node _ :as loc]]
  (if (str/blank? node)
    (zip/remove loc)
    loc))

(defn- remove-comment
  "Remove any strings converted from XML comments from a string node `loc`."
  [[node _ :as loc]]
  (if (and (str/starts-with? node "<!--")
           (str/ends-with? node "-->"))
    (zip/remove loc)
    loc))

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

(defn- edit-leaf
  [[node _ :as loc]]
  (if (string? node)
    (->> loc
         (trim-str)
         (remove-comment))
    loc))

(defn- edit-branch
  [prefix attr-kmap wrapper transformers loc]
  (let [loc* (inject wrapper transformers loc)]
    (if (:replaced? (meta (zip/node loc*)))
      (skip-siblings loc*)
      (->> loc*
           (attr->data-attr)
           (rename-attr attr-kmap)
           (add-prefix prefix)
           (meta-into-attr)))))

(defn- ignore?
  "Return true if it makes sense to ignore this loc."
  [[[tag attr & content :as node] _ :as loc]]
  (and (vector? node)
       (= tag :<>)))                                        ; React fragments

(defn rewrite
  "Process relevant nodes of a zipper made from a `hiccup` tree based on `opts`.
  Return the transformed structure."
  ([hiccup {:keys [prefix attr-kmap wrapper transformers]
            :or   {prefix "rescope"}
            :as   opts}]
   ;; The way hiccup zips, every branch is a hiccup vector, while everything
   ;; else is a leaf. Leafs are usually strings, but can be other types too.
   (let [edit-node (fn [[node _ :as loc]]
                     ;; TODO: remove whitespace before zipping or at least handle leafs first
                     ;; currently whitespace is interfering with pattern matching!
                     (if (vector? node)
                       (edit-branch prefix attr-kmap wrapper transformers loc)
                       (edit-leaf loc)))]
     (loop [loc (hzip/hiccup-zip hiccup)]
       (if (zip/end? loc)
         (zip/root loc)
         (recur (zip/next (if (ignore? loc)
                            loc
                            (edit-node loc))))))))
  ([hiccup]
   (rewrite hiccup nil)))
