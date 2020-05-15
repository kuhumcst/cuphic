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

;; TODO: some way to handle variable length matches, e.g. using `...` symbol?

(defn vector-map-zip
  "Also zips maps in addition to zipping vectors."
  [root]
  (zip/zipper (some-fn vector? (every-pred map? (complement record?)))
              seq
              (fn [node children] (with-meta (vec children) (meta node)))
              root))

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
              (if (s/valid? ::cs/var -)
                (recur (zip/next loc) (assoc ret - +))
                (recur (zip/next loc) ret)))

            (recur (zip/next loc) ret)))))))

(defn- hicv
  "Helper function for normalised destructuring of a hiccup-vector `v`."
  [v]
  (if (map? (second v))
    v
    (into [(first v) {}] (rest v))))

(defn- bindings-delta
  "Get a delta of the local bindings as a map by comparing `cloc` to `hloc`.
  Will return nil if the two nodes do not match."
  [[cnode _ :as cloc] [hnode _ :as hloc]]
  (cond
    ;; Skip directly to the next node.
    (= cnode hnode)
    {}

    ;; Branches (vectors) are the real object of interest.
    (and (vector? cnode)
         (vector? hnode))
    (let [cv (hicv cnode)
          hv (hicv hnode)]
      (if (= (count cv)
             (count hv))
        ;; Return potential local bindings in tag and attr.
        (let [[ctag cattr] cv
              [htag hattr] hv]
          (merge
            (when (s/valid? ::cs/var ctag)
              {ctag htag})
            (attr-bindings cattr hattr)))

        ;; TODO: handle variadic content here
        ;; Fail fast. Nil will bubble up and exit the loop.
        nil))

    ;; TODO: what about strings?
    ;; Leafs get skipped. They are handled as part of the content instead.
    (and (not (vector? cnode))
         (not (vector? cnode)))
    {}))

(defn- potential-match?
  "Helper function for optimising performance."
  [cuphic hiccup]
  ;; Account for the fact that the attr map is optional.
  (<= (dec (count hiccup))
      (count cuphic)
      (inc (count hiccup))))

(defn bindings
  "Get the symbol->value mapping found when comparing `cuphic` to `hiccup`.
  Returns nil if the hiccup does not match the cuphic.

  The two data structures are zipped through in parallel while their bindings
  are collected incrementally."
  [cuphic hiccup]
  (assert (s/valid? ::cs/cuphic cuphic))                    ; elide in prod
  (when (potential-match? cuphic hiccup)
    (loop [cloc (hzip/hiccup-zip cuphic)
           hloc (hzip/hiccup-zip hiccup)
           ret  {}]
      (if (zip/end? cloc)                                   ; TODO: ...and hloc?
        ret
        (when-let [delta (bindings-delta cloc hloc)]
          (recur (zip/next cloc) (zip/next hloc) (merge ret delta)))))))

(defn matches
  "Returns the match, if any, of `hiccup` to `cuphic`."
  [cuphic hiccup]
  (when (bindings cuphic hiccup)
    hiccup))

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
      (walk/postwalk #(get symbol->value % %) to))))


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

(defn- skip-subtree
  "Skip the descendants of the current node."
  [[node _ :as loc]]
  (or (zip/right loc) (zip/rightmost loc)))

(defn- edit-branch
  [prefix attr-kmap wrapper transformers loc]
  (let [loc* (inject wrapper transformers loc)]
    (if (:replaced? (meta (zip/node loc*)))
      (skip-subtree loc*)
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




(comment
  ;; Invalid example
  (bindings '[?tag {:style
                    ;; should fail here
                    {?df ?width}}
              [:p {} "p1"]
              [:p {} "p2"]]
            [:div {:style {:width  "5px"
                           :height "10px"}}
             [:p {} "p1"
              ;; should fail here, but will not reach due to spec assert
              [:glen]]
             [:p {} "p2"]])

  ;; Valid logic var extraction example
  (bindings '[?tag {:style {:width ?width}}
              [:p {} "p1"]
              [:p {} "p2"]]
            [:div {:style {:width  "5px"
                           :height "10px"}}
             [:p {} "p1"]
             [:p {} "p2"]])

  ;; Valid transformation example
  (transform '[?tag {:style {:width ?width}}
               [_ {} "p1"]
               [_ {} "p2"]]

             '[:div
               [?tag {:style {:width ?width}}]
               [:p "width: " ?width]]

             [:span {:style {:width  "5px"
                             :height "10px"}}
              [:p {} "p1"]
              [:p {} "p2"]])

  ;; Valid transformation using an fn as "to" template
  (transform '[?tag {:style {:width ?width}}
               [_ {} "p1"]
               [_ {} "p2"]]

             (fn [{:syms [?tag ?width]}]
               [:div
                [?tag {:style {:width ?width}}]
                [:p "width: " ?width]])

             [:span {:style {:width  "5px"
                             :height "10px"}}
              [:p {} "p1"]
              [:p {} "p2"]])

  ;; should be false
  (s/valid? ::cs/cuphic '[?tag {:style {?df ?width}}
                          [:p {} "p1"]
                          [:p {} "p2"]])

  #_.)

