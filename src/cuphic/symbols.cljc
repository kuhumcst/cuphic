(ns cuphic.symbols
  "These are the special symbols used in Cuphic:

      ?var   -  a variable (binds the corresponding node in the Hiccup to ?var)
      _      -  a wildcard (the slot can be filled by any 1 node)
      ???    -  marks omission of 0 or more nodes
      ...    -  marks omission of 1 or more nodes
      (???)  -  marks repetition of 0 or more nodes (provided as fn args)
      (...)  -  marks repetition of 1 or more nodes (provided as fn args)"
  (:require [clojure.string :as str]))

(defn wildcard?
  [pnode]
  (= pnode '_))

(defn- prefixed-with?
  [sym prefix]
  (str/starts-with? (name sym) prefix))

(defn variable?
  [pnode]
  (and (symbol? pnode)
       (prefixed-with? pnode "?")))

(defn whichever?
  [pnode]
  (or (wildcard? pnode)
      (variable? pnode)))

(defn maybe-omitted?
  [pnode]
  (= pnode '...?))

(defn some-omitted?
  [pnode]
  (= pnode '...))

(defn omitted?
  "Does this `pnode` mark omission?"
  [pnode]
  (or (some-omitted? pnode)
      (maybe-omitted? pnode)))

(defn maybe-repeated?
  [pnode]
  (and (list? pnode)
       (= '...? (first pnode))))

(defn some-repeated?
  [pnode]
  (and (list? pnode)
       (= '... (first pnode))))

(defn repeated?
  "Does this `pnode` mark repetition?"
  [pnode]
  (or (some-repeated? pnode)
      (maybe-repeated? pnode)))

(defn quantified?
  "Does this `pnode` mark omission or repetition?"
  [pnode]
  (or (omitted? pnode)
      (repeated? pnode)))

(defn optional?
  "Does this `pnode` allow for 0 cases? I.e. will the full pattern still be able
  to be matched even if pnode is represented by 0 nodes?"
  [pnode]
  (or (maybe-omitted? pnode)
      (maybe-repeated? pnode)))

(defn arbitrary?
  "Will this `pnode` match nodes arbitrarily?"
  [pnode]
  (or (wildcard? pnode)
      (variable? pnode)
      (omitted? pnode)
      (and (repeated? pnode)
           (every? arbitrary? (rest pnode)))))
