(ns cuphic.xml
  "Clojure(Script) implementation of an XML parser that, more or less directly,
  converts an XML file into Hiccup data without trying to be clever about it.

  Comments and superfluous whitespace are deliberately not preserved, while
  namespaces are converted into regular Clojure namespaces. No attempt is made
  to map namespace URIs to namespace aliases.

  See: https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeType"
  (:require [clojure.string :as str])
  #?(:clj (:import [org.w3c.dom Document Element Text Comment Node]
                   [javax.xml.parsers DocumentBuilderFactory DocumentBuilder]
                   [java.io ByteArrayInputStream InputStream File])))

#?(:clj  (defn iter-nodes
           [nodes]
           (for [n (range (.getLength nodes))]
             (.item nodes n)))
   :cljs (do
           (def Document js/Document)
           (def Element js/Element)
           (def Text js/Text)
           (def Comment js/Comment)
           (def Node js/Node)))

(def parser
  #?(:clj  (.newDocumentBuilder (DocumentBuilderFactory/newInstance))
     :cljs (js/DOMParser.)))

(defn keywordize
  "Keywordize `s` converting XML namespaces to Clojure namespaces."
  [s]
  (let [[s1 s2] (str/split s #":")]
    (if s2
      (keyword s1 s2)
      (keyword s1))))

(defn dom-parse
  "Parse `xml` into a DOM tree."
  [xml]
  #?(:clj  (cond
             (string? xml)
             (dom-parse (ByteArrayInputStream. (.getBytes xml)))

             (instance? InputStream xml)
             (.parse ^DocumentBuilder parser ^InputStream xml)

             (instance? File xml)
             (.parse ^DocumentBuilder parser ^File xml))

     :cljs (.-firstChild (.parseFromString parser xml "text/xml"))))

(defn node-tag
  "Get a Hiccup tag from a `node`."
  [^Node node]
  (keywordize #?(:clj  (.getNodeName node)
                 :cljs (.-tagName node))))

(defn node-attrs
  "Get a Hiccup attributes map from a `node`."
  [^Node node]
  #?(:clj  (let [attributes (iter-nodes (.getAttributes node))]
             (into {} (for [attribute attributes]
                        [(keywordize (.getNodeName attribute))
                         (.getNodeValue attribute)])))
     :cljs (let [attributes (.-attributes node)]
             (into {} (for [attribute attributes]
                        [(keywordize (.-name attribute))
                         (.-value attribute)])))))

(defn node-content
  "Get the children of the `node` as objects."
  [^Node node]
  #?(:clj  (iter-nodes (.getChildNodes node))
     :cljs (.-childNodes node)))

(defn node-data
  "Return the data of a `node`. Mimics the return value of clojure.data.xml."
  [^Node node]
  {:tag     (node-tag node)
   :attrs   (node-attrs node)
   :content (node-content node)})

(defn- whole-text
  [^Text node]
  #?(:clj  (.getWholeText node)
     :cljs (.-wholeText node)))

(defn node->hiccup
  "Recursively convert a `node` and its children to Hiccup."
  [node]
  (condp instance? node

    Document
    #?(:clj  (node->hiccup
               (doto (.getDocumentElement node)
                 (.normalize)))

       ;; TODO: is the CLJS part relevant?
       :cljs (do
               (map node->hiccup (node-content node))))

    Element
    (let [{:keys [tag attrs content]} (node-data node)]
      (->> (map node->hiccup content)
           (remove nil?)
           (into [tag attrs])))

    ;; TODO: should probably escape HTML here
    Text
    (let [s (whole-text node)]
      (when (not (str/blank? s))
        s))

    Comment
    nil

    :else node))

(defn parse
  "Parse `xml` as hiccup data."
  [xml]
  (node->hiccup (dom-parse xml)))

(comment
  (-> (clojure.java.io/resource "examples/tei/test-1307-anno-tei.xml")
      (clojure.java.io/file)
      (parse))
  #_.)
