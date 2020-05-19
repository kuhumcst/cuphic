(ns cuphic.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn- prefixed-with?
  [s]
  (comp #(str/starts-with? % s) name))                      ; CLJS needs name

;; Every symbol prepended with ? has a single value captured in its place.
(s/def ::?
  (s/and symbol? (prefixed-with? "?")))

(s/def ::*
  (s/and symbol? (prefixed-with? "*")))

(s/def ::+
  (s/and symbol? (prefixed-with? "+")))

;; Every symbol prepended with * or + is a placeholder for multiple items.
(s/def ::quantifier
  (s/or :* ::*
        :+ ::+))

;; If the prepended symbols are not given a specific name, their values are not
;; captured, but rather ignored.
(s/def ::ignored
  (s/or
    :? #{'?}
    :* #{'*}
    :+ #{'+}))

;; Possible insertion points for single value placeholders and quantifiers.
;; Cannot be used in place of collections, e.g. hiccup vectors or attr maps!
(s/def ::slot
  (s/or
    :ignored ::ignored
    :placeholder ::?
    :quantifier ::quantifier
    :value (complement coll?)))

(s/def ::attr
  (s/map-of keyword? (s/or
                       :slot ::slot
                       :map ::attr)))

;; Conforms to a superset of regular hiccup.
(s/def ::cuphic
  (s/and vector?
         (s/conformer vec vec)                              ; unform as vector
         (s/cat
           :tag ::slot
           :attr (s/? ::attr)
           :content (s/* (s/or
                           :cuphic ::cuphic
                           :slot ::slot
                           :other (complement map?))))))

(comment
  ;; TODO: should be invalid according to spec
  (s/conform ::cuphic '[*])
  #_.)
