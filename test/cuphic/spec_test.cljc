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

;; Includes _ (ignored values), meaning it can only be a "from" pattern.
(def from
  '[?div {:class ?class
          :id    ?
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
    ;; The common single-value placeholder.
    (is (= [:placeholder '?single-value]
           (s/conform ::cs/slot '?single-value)))

    ;; Quantifiers are for (potentially) multiple values.
    (is (= [:quantifier [:* '*zero-or-more]]
           (s/conform ::cs/slot '*zero-or-more)))
    (is (= [:quantifier [:+ '+one-or-more]]
           (s/conform ::cs/slot '+one-or-more)))

    ;; Single-value placeholders and quantifiers are ignored (= not captured)
    ;; unless they are given a specific name.
    (is (= [:ignored [:? '?]]
           (s/conform ::cs/slot '?)))
    (is (= [:ignored [:* '*]]
           (s/conform ::cs/slot '*)))
    (is (= [:ignored [:+ '+]]
           (s/conform ::cs/slot '+)))

    ;; Regular values like, other symbols and strings, should pass through.
    (is (= [:value 'symbol]
           (s/conform ::cs/slot 'symbol)))
    (is (= [:value "string"]
           (s/conform ::cs/slot "string")))

    ;; Collections should not conform (covered by the full Cuphic spec itself).
    (is (not (s/valid? ::cs/slot [:div]))))

  (testing "complex hiccup"
    (is (= '{:tag     [:value :div],
             :attr    {:class [:slot [:value "class"]]
                       :id    [:slot [:value "id"]]
                       :style [:map
                               {:width  [:slot [:value "10px"]]
                                :height [:slot [:value "20px"]]}]}
             :content [[:cuphic
                        {:tag     [:value :p],
                         :attr    {:on-click [:slot [:value do-stuff]]}
                         :content [[:slot [:value "text"]]]}]
                       [:cuphic
                        {:tag     [:value :p]
                         :content [[:slot [:value "more text"]]
                                   [:slot [:value 1]]
                                   [:slot [:value 2]]
                                   [:slot [:value 3]]]}]
                       [:cuphic
                        {:tag     [:value :p]
                         :content [[:slot [:value "text"]]
                                   [:cuphic {:tag     [:value :span]
                                             :content [[:slot [:value "x"]]]}]
                                   [:cuphic {:tag     [:value :em]
                                             :content [[:slot [:value "y"]]]}]]}]]}
           (s/conform ::cs/cuphic hiccup-example))))

  (testing "complex cuphic"
    (is (= '{:tag     [:placeholder ?div]
             :attr    {:class [:slot [:placeholder ?class]]
                       :id    [:slot [:ignored [:? ?]]]
                       :style [:map {:width  [:slot [:value "10px"]]
                                     :height [:slot [:value "20px"]]}]}
             :content [[:cuphic {:tag     [:value :p]
                                 :attr    {:on-click [:slot [:value do-stuff]]}
                                 :content [[:slot [:value "text"]]]}]
                       [:cuphic {:tag     [:placeholder ?p]
                                 :content [[:slot [:value "more text"]]
                                           [:slot [:value 1]]
                                           [:slot [:value 2]]
                                           [:slot [:value 3]]]}]
                       [:cuphic {:tag     [:value :p]
                                 :content [[:slot [:value "text"]]
                                           [:cuphic {:tag     [:placeholder ?span]
                                                     :content [[:slot [:value "x"]]]}]
                                           [:cuphic {:tag     [:value :em]
                                                     :content [[:slot [:value "y"]]]}]]}]]}
           (s/conform ::cs/cuphic from)))))
