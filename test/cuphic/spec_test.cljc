(ns cuphic.spec-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [cuphic.spec :as cs]))

(def hiccup-example
  '[:div {:class "class"
          :id    "id"
          :style {:width  "10px"
                  :height "20px"}}
    [:p {:on-click do-stuff}
     "text"]
    [:p "more text" 1 2 3]
    [:p "text"
     [:span "x"]
     [:em "y"]]])

;; Includes _ (ignored values), meaning it can only be a "from" template.
(def from
  '[?div {:class ?class
          :id    _
          :style {:width  "10px"
                  :height "20px"}}
    [:p {:on-click do-stuff}
     "text"]
    [?p "more text" 1 2 3]
    [:p "text"
     [?span "x"]
     [:em "y"]]])

(def symbol->value
  '{?div   :div
    ?class "class"
    ?p     :p
    ?span  :span})

(deftest spec-validation
  (testing "slots (insertion points for symbols)"
    (is (= [:var '?var]
           (s/conform ::cs/slot '?var)))
    (is (= [:ignored '_ignored]
           (s/conform ::cs/slot '_ignored)))
    (is (= [:other 'symbol]
           (s/conform ::cs/slot 'symbol)))

    (is (= [:other "string"]
           (s/conform ::cs/slot "string")))
    (is (not (s/valid? ::cs/slot [:div]))))

  (testing "complex hiccup"
    (is (= '{:tag     [:other :div]
             :attr    {:class [:slot [:other "class"]]
                       :id    [:slot [:other "id"]]
                       :style [:map {:width  [:slot [:other "10px"]]
                                     :height [:slot [:other "20px"]]}]}
             :content [[:cuphic {:tag     [:other :p]
                                 :attr    {:on-click [:slot [:other do-stuff]]}
                                 :content [[:other "text"]]}]
                       [:cuphic {:tag     [:other :p]
                                 :content [[:other "more text"]
                                           [:other 1]
                                           [:other 2]
                                           [:other 3]]}]
                       [:cuphic
                        {:tag     [:other :p],
                         :content [[:other "text"]
                                   [:cuphic {:tag     [:other :span]
                                             :content [[:other "x"]]}]
                                   [:cuphic {:tag     [:other :em]
                                             :content [[:other "y"]]}]]}]]}
           (s/conform ::cs/cuphic hiccup-example))))

  (testing "complex cuphic"
    (is (= '{:tag     [:var ?div]
             :attr    {:class [:slot [:var ?class]]
                       :id    [:slot [:ignored _]]
                       :style [:map {:width  [:slot [:other "10px"]]
                                     :height [:slot [:other "20px"]]}]}
             :content [[:cuphic {:tag     [:other :p]
                                 :attr    {:on-click [:slot [:other do-stuff]]}
                                 :content [[:other "text"]]}]
                       [:cuphic {:tag     [:var ?p]
                                 :content [[:other "more text"]
                                           [:other 1]
                                           [:other 2]
                                           [:other 3]]}]
                       [:cuphic
                        {:tag     [:other :p],
                         :content [[:other "text"]
                                   [:cuphic {:tag     [:var ?span]
                                             :content [[:other "x"]]}]
                                   [:cuphic {:tag     [:other :em]
                                             :content [[:other "y"]]}]]}]]}
           (s/conform ::cs/cuphic from)))))
