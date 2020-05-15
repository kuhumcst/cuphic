(ns cuphic.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; Every symbol prepended with ? has a value captured in its place.
(s/def ::var
  (s/and symbol?
         (comp #(str/starts-with? % "?") name)))            ; CLJS needs name

;; Everything prepended with _ is ignored.
(s/def ::ignored
  (s/and symbol?
         (comp #(str/starts-with? % "_") name)))            ; CLJS needs name

;; Possible insertion points for logic variables and other symbols.
;; Cannot be used in place of collections, e.g. hiccup vectors or attr maps!
(s/def ::slot
  (s/or
    :var ::var
    :ignored ::ignored
    :other (complement coll?)))

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
                           :other (complement map?))))))
