(ns cuphic.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [cuphic.core :as cup]))

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

;; Includes _ (ignored values) meaning it can only be a "from" template.
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

(deftest bindings
  (testing "basic symbol->value bindings"
    (is (= symbol->value
           (cup/bindings from hiccup-example))))

  (testing "hiccup source in metadata"
    (testing "basic symbol->value bindings"
      (is (= hiccup-example
             (:source (meta (cup/bindings from hiccup-example)))))))

  (testing "without optional attr"
    (is (= '{?tag :div}
           (cup/bindings '[?tag "text"] [:div {:class "class"} "text"]))))

  (testing "quantifiers"
    (testing "affixed"
      (testing "+"
        (is (= '{?tag :div +rest ["2" "3"]}
               (cup/bindings '[?tag "1" +rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "1" +rest ["2" "3"]}
               (cup/bindings '[?tag ?other +rest] [:div "1" "2" "3"])))
        (is (= nil
               (cup/bindings '[?tag "1" +rest] [:div "1"]))))

      (testing "*"
        (is (= '{?tag :div *rest ["2" "3"]}
               (cup/bindings '[?tag "1" *rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "1" *rest ["2" "3"]}
               (cup/bindings '[?tag ?other *rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div}
               (cup/bindings '[?tag "1" *rest] [:div "1"])))))

    (testing "prefixed"
      (testing "+"
        (is (= '{?tag :div +rest ["1" "2"]}
               (cup/bindings '[?tag +rest "3"] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "3" +rest ["1" "2"]}
               (cup/bindings '[?tag +rest ?other] [:div "1" "2" "3"])))
        (is (= nil
               (cup/bindings '[?tag +rest "1"] [:div "1"]))))

      (testing "*"
        (is (= '{?tag :div *rest ["1" "2"]}
               (cup/bindings '[?tag *rest "3"] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "3" *rest ["1" "2"]}
               (cup/bindings '[?tag *rest ?other] [:div "1" "2" "3"])))
        (is (= '{?tag :div}
               (cup/bindings '[?tag *rest "1"] [:div "1"])))))

    (testing "infixed"
      (testing "+"
        (= '{+rest [4 5 6 7] ?tag :1 ?2 2 ?3 3 ?8 8 ?9 9}
           (cup/bindings '[?tag ?2 ?3 +rest ?8 ?9]
                         [:1 2 3 4 5 6 7 8 9]))
        (= nil
           (cup/bindings '[?tag ?2 ?3 +rest ?8 ?9]
                         [:1 2 3 8 9])))

      (testing "*"
        (= '{*rest [4 5 6 7] ?tag :1 ?2 2 ?3 3 ?8 8 ?9 9}
           (cup/bindings '[?tag ?2 ?3 *rest ?8 ?9]
                         [:1 2 3 4 5 6 7 8 9]))
        (= '{?tag :1 ?2 2 ?3 3 ?8 8 ?9 9}
           (cup/bindings '[?tag ?2 ?3 *rest ?8 ?9]
                         [:1 2 3 8 9]))))))

(deftest matches
  (testing "matches function (based on bindings)"
    (is (= hiccup-example
           (cup/matches from hiccup-example)))))

(deftest transform
  (testing "preservation"
    (is (= hiccup-example
           (cup/transform from
                          '[?div {:class ?class
                                  :id    "id"
                                  :style {:width  "10px"
                                          :height "20px"}}
                            [:p {:on-click do-stuff}
                             "text"]
                            [?p "more text" 1 2 3]
                            [:p "text"
                             [?span "x"]
                             [:em "y"]]]
                          hiccup-example))))

  (testing "cuphic/cuphic transformation"
    (is (= [:div [:p] [:span {:class "class"}]]
           (cup/transform from
                          '[?div [?p] [?span {:class ?class}]]
                          hiccup-example))))

  (testing "fn/cuphic transformation"
    (is (= [:div [:p] [:span {:class "class"}]]
           (cup/transform (fn [hiccup] symbol->value)
                          '[?div [?p] [?span {:class ?class}]]
                          hiccup-example))))

  (testing "cuphic/fn transformation"
    (is (= [:div [:p] [:span {:class "class"}]]
           (cup/transform from
                          (fn [{:syms [?div ?p ?span ?class]}]
                            [?div [?p] [?span {:class ?class}]])
                          hiccup-example))))

  (testing "fn/fn transformation"
    (is (= [:div [:p] [:span {:class "class"}]]
           (cup/transform (fn [hiccup] symbol->value)
                          (fn [{:syms [?div ?p ?span ?class]}]
                            [?div [?p] [?span {:class ?class}]])
                          hiccup-example)))))
