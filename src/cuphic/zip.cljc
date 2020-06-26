(ns cuphic.zip
  (:require [clojure.zip :as zip]))

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

(defn multi-replace
  "Replace `loc` with `nodes`."
  [loc nodes]
  (zip/remove (reduce zip/insert-left loc nodes)))
