(ns user
  (:require [clojure.pprint :refer [pprint]]
            [shadow.resource :as resource]
            [reagent.dom :as rdom]
            [meander.epsilon :as m]
            [cuphic.core :as cup]
            [recap.widgets.carousel :as carousel]
            [recap.css :as rcss]
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
  (str rcss/shadow-style
       "\n\n/*\n\t === tei.css ===\n*/\n"
       tei-css))

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
  (cup/transformer
    :from '[:list +items]
    :to (fn [{:syms [+items]}]
          (into [:ul] (for [[tag attr & content] +items]
                        (into [:li] content))))))

(def ref-as-anchor
  (cup/transformer
    :from '[? {:ref  ?ref
               :type ?type} *]
    :to (fn [{:syms [?ref ?type]}]
          ;; TODO: bug in attr-bindings - now need to check for attr existence
          (when ?ref
            [:a {:href  ?ref
                 :title (da-type ?type)}
             [:slot]]))))

;; In order to represent pbs as slides in a carousel their boundaries must be
;; made explicit in the HTML structure.
(def wrap-pbs
  (cup/transformer
    :from '[:div * [:<> [:pb] +]]
    :to (fn [{:syms [<>] :as bindings}]
          (let [{:keys [begin end]} (meta <>)
                source (:source (meta bindings))]
            (vec (concat (subvec source 0 begin)
                         [(into [:slides] (->> (subvec source begin end)
                                               (partition-by #(= :pb (first %)))
                                               (partition 2)
                                               (map (partial apply concat))
                                               (map #(into [:slide] %))))]
                         (subvec source end)))))))

(def default-fn
  (helpers/default-fn {:prefix    "tei"
                       :attr-kmap {:xml:lang :lang
                                   :xml:id   :id}}))

(defn custom-wrapper
  [old-node new-node]
  (let [styled-slide (constantly [:<> [:style recap+tei-css] new-node])]
    (vary-meta old-node assoc :ref (rescope/shadow-ref styled-slide))))

(def internal-stage
  {:transformers [ref-as-anchor
                  list-as-ul]
   :wrapper      custom-wrapper
   :default      default-fn})

;; TODO: no CSS applied due to shadow root - how to fix?
;; TODO: illegal HTML tags since HTML displayed in shadow root come directly from captured bindings
;; TODO: React key warnings, forced div use (fix in recap)
(def carousel-pbs
  (cup/transformer
    :from '[:slides [:<> [:slide +content]]]
    :to (fn [{:syms [<>]}]
          [carousel/carousel {:i   0
                              :kvs (mapv (fn [page]
                                           ["(missing page label)"
                                            (into [:<>] (->> (get page '+content)
                                                             (map #(cup/rewrite % [internal-stage]))))])
                                         <>)}])))

(defonce css-href
  (interop/auto-revoked (atom nil)))

(def all-stages
  [{:transformers [wrap-pbs]}
   {:transformers [ref-as-anchor
                   list-as-ul
                   carousel-pbs]
    :wrapper      custom-wrapper
    :default      default-fn}])

(defn app
  []
  (let [hiccup     (-> (xml/parse tei-example)
                       (cup/rewrite all-stages))
        teiheader  (select/one hiccup (select/element :tei-teiheader))
        facsimile  (select/one hiccup (select/element :tei-facsimile))
        text       (select/one hiccup (select/element :tei-text))
        test-nodes (select/all hiccup
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
       [:summary "CSS"]
       [:pre tei-css]]
      ;; This is just left here as a proof of concept. Using <link> over <style>
      ;; in a shadow DOM introduces a bit of flickering in Chrome and Firefox,
      ;; which is not desirable. Safari seems unaffected, though.
      ;[rescope/scope hiccup [:link {:rel "stylesheet" :href @css-href}]]]
      [rescope/scope hiccup tei-css]]
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
