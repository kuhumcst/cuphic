(ns user
  (:require [clojure.pprint :refer [pprint]]
            [shadow.resource :as resource]
            [reagent.dom :as rdom]
            [meander.epsilon :as m]
            [cuphic.core :as cup]
            [dk.cst.stucco.plastic :as plastic]
            [dk.cst.stucco.util.css :as stucco-css]
            [rescope.helpers :as helpers]
            [rescope.formats.xml :as xml]
            [rescope.select :as select]
            [rescope.style :as style]
            [rescope.interop :as interop]
            [rescope.core :as rescope]))

(def tei-example
  ;(resource/inline "examples/tei/1151anno-anno-tei.xml"))
  ;(resource/inline "examples/tei/tei_example.xml"))
  (resource/inline "examples/tei/test-1307-anno-tei.xml"))

(def css-example
  (resource/inline "examples/css/tei.css"))

(def tei-css
  (style/prefix-css "tei" css-example))

(def recap+tei-css
  (str stucco-css/shadow-style "\n\n/*\n\t === tei.css ===\n*/\n" tei-css))

(defn da-type
  [type]
  (let [type->s {"conference" "denne konference"
                 "org"        "denne organisation"
                 "pers"       "denne person"
                 "place"      "dette sted"
                 "publ"       "denne publikation"
                 "receiver"   "denne modtager"
                 "sender"     "denne afsender"}]
    (str "Vis mere om " (type->s type "dette"))))

(defn meander-transformer
  [x]
  (m/rewrite x
    [:list (m/or {:as ?attr}
                 (m/let [?attr {}])) .
     ;; TODO: fix - only works on :item with exactly 1 piece of content
     [:item {:as _} !x] ...]
    [:ul ?attr .
     [:li !x] ...]

    [_ {:ref  (m/some ?ref)
        :type ?type} & _]
    [:a {:href  ?ref
         :title (m/app da-type ?type)}
     [:slot]]))

(def list-as-ul
  (cup/->transformer
    '[:list +items]

    (fn [{:syms [+items]}]
      (into [:ul] (for [[tag attr & content] +items]
                    (into [:li] content))))))

(def ref-as-anchor
  (cup/->transformer
    '[? {:ref  ?ref
         :type ?type} *]

    (fn [{:syms [?ref ?type]}]
      ;; TODO: bug in attr-bindings - now need to check for attr existence
      (when ?ref
        [:a {:href  ?ref
             :title (da-type ?type)}
         [:slot]]))))

(def default-fn
  (helpers/default-fn {:prefix    "tei"
                       :attr-kmap {:xml:lang :lang
                                   :xml:id   :id}}))

(defn custom-wrapper
  [old-node new-node]
  (let [styled-slide (constantly [:<> [:style recap+tei-css] new-node])]
    (vary-meta old-node assoc :ref (rescope/shadow-ref styled-slide))))

(def inner-stage
  {:transformers [ref-as-anchor
                  list-as-ul]
   :wrapper      custom-wrapper
   :default      default-fn})

;; Fairly complex transformer that restructures sibling-level page content into
;; an interactive carousel recap component. The large amount of content captured
;; as page content has to be explicitly rewritten in a separate call. Otherwise,
;; it will be skipped entirely.
(def carousel-pbs
  (cup/->transformer
    '[:div * [:<> [:pb] +]]

    (fn [{:syms [<>] :as bindings}]
      (let [{:keys [begin end]} (meta <>)
            source     (:source (meta bindings))
            pages      (->> (subvec source begin end)
                            (partition-by #(= :pb (first %)))
                            (partition 2)
                            (map (partial apply concat)))
            pp         (count pages)
            kvs        (for [[[_ {:keys [n facs]}] :as page] pages]
                         [(str "Side " n " af " pp "; facs. " facs ".")
                          page])
            rewrite-kv (fn [[k v]]
                         (let [rewrite #(cup/rewrite % inner-stage)]
                           [k (into [:<>] (map rewrite v))]))]
        [:<>
         [cup/rewrite (subvec source 0 begin) inner-stage]
         [plastic/carousel
          {:i   0
           :kvs (map rewrite-kv kvs)}
          {:aria-label "Facsimile"}]]))))

(defonce css-href
  (interop/auto-revoked (atom nil)))

(def outer-stage
  {:transformers [ref-as-anchor
                  list-as-ul
                  carousel-pbs]
   :wrapper      custom-wrapper
   :default      default-fn})

(defn app
  []
  (let [hiccup     (xml/parse tei-example)
        hiccup*    (cup/rewrite hiccup outer-stage)
        teiheader  (select/one hiccup* (select/element :tei-teiheader))
        facsimile  (select/one hiccup* (select/element :tei-facsimile))
        text       (select/one hiccup* (select/element :tei-text))
        test-nodes (select/all hiccup*
                               (select/element :tei-forename)
                               (select/attr {:data-type "first"}))]
    (reset! css-href (interop/blob-url [tei-css] {:type "text/css"}))
    [:<>
     [:fieldset
      [:legend "Document"]
      [:details
       [:summary "Hiccup"]
       [:pre (with-out-str (pprint hiccup))]]
      [:details
       [:summary "Hiccup*"]
       [:pre (with-out-str (pprint hiccup*))]]
      [:details
       [:summary "CSS"]
       [:pre tei-css]]

      ;; Test searching for terms
      [:pre (with-out-str (pprint (cup/scrape hiccup
                                              '[?tag {:when ?when}]
                                              '[?tag {:when "2020"}])))]
      ;; Test scanning for terms
      #_[:pre (with-out-str (pprint (cup/scan hiccup
                                              '[?tag {:when ?when}]
                                              '[?tag {:when "2020"}])))]

      ;; This is just left here as a proof of concept. Using <link> over <style>
      ;; in a shadow DOM introduces a bit of flickering in Chrome and Firefox,
      ;; which is not desirable. Safari seems unaffected, though.
      ;[rescope/scope hiccup [:link {:rel "stylesheet" :href @css-href}]]]
      [rescope/scope hiccup* tei-css]]
     [:fieldset
      [:legend "Header"]
      [:details
       [:summary "Hiccup"]
       [:pre (with-out-str (pprint teiheader))]]]
     [:fieldset
      [:legend "Facsimile"]
      [:details
       [:summary "Hiccup"]
       [:pre (with-out-str (pprint facsimile))]]]
     [:fieldset
      [:legend "Text"]
      [:details
       [:summary "Hiccup"]
       [:pre (with-out-str (pprint text))]]]
     [:fieldset
      [:legend "Test output"]
      [:pre (with-out-str (pprint test-nodes))]]]))

(def root
  (js/document.getElementById "app"))

(defn ^:dev/after-load render
  []
  (rdom/render [app] root))

(defn start-dev
  []
  (println "Started development environment for Cuphic.")
  (render))
