(ns dk.cst.cuphic.symbols
  "These are the special symbols used in Cuphic:

      ?      -  optional node (the slot can be filled by a node -OR- be empty)
      _      -  wildcard (the slot can be filled by any 1 node)
      ???    -  marks omission of 0 or more nodes
      ...    -  marks omission of 1 or more nodes
      (???)  -  marks repetition of 0 or more nodes (provided as fn args)
      (...)  -  marks repetition of 1 or more nodes (provided as fn args)
      var    -  variable (binds the corresponding node in the Hiccup to 'var')
      ?var   -  optional variable, i.e. the slot can also be empty"
  (:require [clojure.string :as str]))

(def wildcard
  "Marks a slot that can be filled by any 1 node."
  '_)

(def optional
  "Marks a slot that can be filled any 1 node -OR- be empty."
  '?)

(def omission
  "Marks omission of 1 or more nodes."
  '...)

(def optional-omission
  "Marks omission of 0 or more nodes."
  '???)

(defn- prefixed-with?
  [sym prefix]
  (str/starts-with? (name sym) prefix))

(defn wildcard?
  [pnode]
  (= pnode wildcard))

(defn optional?
  [pnode]
  (= pnode optional))

(defn variable?
  [pnode]
  (and (symbol? pnode)
       (not (#{wildcard optional omission optional-omission} pnode))
       (not (prefixed-with? pnode "?"))))

(defn optional-variable?
  [pnode]
  (and (symbol? pnode)
       (not (#{optional optional-omission} pnode))
       (prefixed-with? pnode "?")))

(defn slot?
  [pnode]
  (symbol? pnode))

(defn optional-omission?
  [pnode]
  (= pnode optional-omission))

(defn definite-omission?
  [pnode]
  (= pnode omission))

(defn omission?
  "Does this `pnode` mark omission?"
  [pnode]
  (or (definite-omission? pnode)
      (optional-omission? pnode)))

(defn optional-repetition?
  [pnode]
  (and (list? pnode)
       (= optional-omission (first pnode))))

(defn definite-repetition?
  [pnode]
  (and (list? pnode)
       (= omission (first pnode))))

(defn repetition?
  "Does this `pnode` mark repetition?"
  [pnode]
  (or (definite-repetition? pnode)
      (optional-repetition? pnode)))

(defn quantification?
  "Does this `pnode` mark omission or repetition?"
  [pnode]
  (or (omission? pnode)
      (repetition? pnode)))

(defn optional-quantification?
  "Does this `pnode` allow for 0 cases? I.e. will the full pattern still be able
  to be matched even if pnode is represented by 0 nodes?"
  [pnode]
  (or (optional-omission? pnode)
      (optional-repetition? pnode)))

(defn arbitrary?
  "Will this `pnode` match nodes arbitrarily?"
  [pnode]
  (or (wildcard? pnode)
      (variable? pnode)
      (optional-quantification? pnode)
      (and (repetition? pnode)
           (every? arbitrary? (rest pnode)))))

(defn slot-type
  [pnode]
  (if (list? pnode)
    (condp = (first pnode)
      omission :definite-repetition
      optional-omission :optional-repetition)
    (cond
     (optional? pnode) :optional
     (wildcard? pnode) :wildcard
     (optional-omission? pnode) :optional-omission
     (definite-omission? pnode) :definite-omission
     (optional-variable? pnode) :optional-variable
     (variable? pnode) :variable)))
