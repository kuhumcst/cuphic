(ns dk.cst.cuphic.zip
  "Generic zipper functions."
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

(defn iterate-zipper
  "Lazily iterate through all zipper states from the given `loc` to the end."
  [loc]
  (take-while (complement zip/end?) (iterate zip/next loc)))

(defn reduce-zipper
  "Traverse the zipper starting from `loc`, applying `f` to all locs on the way.
  Once the whole zipper has been traversed, zip up and return the changed tree."
  [f loc]
  (loop [[node :as loc] loc]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (f loc node))))))

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
  [loc [node & nodes]]
  (reduce zip/insert-right (zip/replace loc node) (reverse nodes)))
