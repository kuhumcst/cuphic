(ns kuhumcst.rescope.core
  "Reagent components for integrating with the shadow DOM."
  (:require [clojure.string :as str]
            [reagent.dom :as rdom]
            [kuhumcst.rescope.interop :as interop]
            [kuhumcst.rescope.select :as select]))

(def custom-tag
  #"\w+(-\w+)+")

;; https://html.spec.whatwg.org/multipage/custom-elements.html#valid-custom-element-name
(def hyphen-tags
  #{"annotation-xml"
    "color-profile"
    "font-face"
    "font-face-src"
    "font-face-uri"
    "font-face-format"
    "font-face-name"
    "missing-glyph"})

(defn hiccup->custom-tags
  "Get a set of all custom tags (as strings) found in a `hiccup` tree."
  [hiccup]
  (->> (select/all hiccup)
       (map (comp str/lower-case name first))
       (remove hyphen-tags)
       (filter (partial re-matches custom-tag))
       (set)))

(defn define-elements!
  "Define custom HTML elements covering all `tags`."
  [tags]
  (doseq [tag tags]
    (interop/define-element! tag)))

(defn shadow-ref
  "Get a :ref fn for a DOM element to render a given `comp` as its shadow root.
  The component should accept a single argument: the element's DOM reference."
  [comp]
  (fn [this]
    (when this
      ;; TODO: this extra check was suddenly necessary - investigate
      (when (undefined? (.-shadow this))
        (set! (.-shadow this) (.attachShadow this #js{:mode "open"})))
      (rdom/render [comp this] (.-shadow this)))))

(defn shadow-wrapper
  [old-node new-node]
  (vary-meta old-node assoc :ref (shadow-ref (constantly new-node))))

(defn scope
  "Render `hiccup` inside a shadow DOM with the root element as the shadow host.
  Optionally takes scoped `css` to apply to the content inside the shadow DOM.
  The `css` can be a string or hiccup, e.g. [:style], [:link], [:template]."
  ([hiccup & [css]]
   (define-elements! (hiccup->custom-tags hiccup))
   (let [[[tag attr] children] (if (map? (second hiccup))
                                 (split-at 2 hiccup)
                                 (split-at 1 hiccup))
         style (if (string? css) [:style css] css)
         comp  (fn [_] (into [:<> style] children))]
     [tag (assoc attr :ref (shadow-ref comp))])))
