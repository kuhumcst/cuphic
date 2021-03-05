(ns cuphic.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [cuphic.core :refer [get-bindings matches transform scan scrape]]))

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

;; Includes _ (ignored values) meaning it can only be a "from" pattern.
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

(deftest test-bindings
  (testing "basic symbol->value bindings"
    (is (= symbol->value
           (get-bindings from hiccup-example))))

  (testing "hiccup source in metadata"
    (testing "basic symbol->value bindings"
      (is (= hiccup-example
             (:source (meta (get-bindings from hiccup-example)))))))

  (testing "without optional attr"
    (is (= '{?tag :div}
           (get-bindings '[?tag "text"] [:div {:class "class"} "text"]))))

  (testing "quantifiers"
    (testing "affixed"
      (testing "+"
        (is (= '{?tag :div +rest ["2" "3"]}
               (get-bindings '[?tag "1" +rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "1" +rest ["3"]}
               (get-bindings '[?tag ?other "2" +rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div +rest ["2" "3"] ?other "1"}
               (get-bindings '[?tag ?other +rest] [:div "1" "2" "3"])))
        (is (= nil
               (get-bindings '[?tag "1" +rest] [:div "1"]))))

      (testing "*"
        (is (= '{?tag :div *rest ["2" "3"]}
               (get-bindings '[?tag "1" *rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "1" *rest ["3"]}
               (get-bindings '[?tag ?other "2" *rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div *rest ["2" "3"] ?other "1"}
               (get-bindings '[?tag ?other *rest] [:div "1" "2" "3"])))
        (is (= '{?tag :div *rest []}
               (get-bindings '[?tag "1" *rest] [:div "1"])))))

    (testing "prefixed"
      (testing "+"
        (is (= '{?tag :div +rest ["1" "2"]}
               (get-bindings '[?tag +rest "3"] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "3" +rest ["1"]}
               (get-bindings '[?tag +rest "2" ?other] [:div "1" "2" "3"])))
        (is (= '{?tag :div +rest ["1" "2"] ?other "3"}
               (get-bindings '[?tag +rest ?other] [:div "1" "2" "3"])))
        (is (= nil
               (get-bindings '[?tag +rest "1"] [:div "1"]))))

      (testing "*"
        (is (= '{?tag :div *rest ["1" "2"]}
               (get-bindings '[?tag *rest "3"] [:div "1" "2" "3"])))
        (is (= '{?tag :div ?other "3" *rest ["1"]}
               (get-bindings '[?tag *rest "2" ?other] [:div "1" "2" "3"])))
        (is (= '{?tag :div *rest ["1" "2"] ?other "3"}
               (get-bindings '[?tag *rest ?other] [:div "1" "2" "3"])))
        (is (= '{?tag :div *rest []}
               (get-bindings '[?tag *rest "1"] [:div "1"])))))

    (testing "infixed"
      (testing "+"
        (= '{+rest [4 5 6 7] ?tag :1 ?2 2 ?3 3 ?8 8 ?9 9}
           (get-bindings '[?tag ?2 ?3 +rest ?8 ?9]
                         [:1 2 3 4 5 6 7 8 9]))
        (= nil
           (get-bindings '[?tag ?2 ?3 +rest ?8 ?9]
                         [:1 2 3 8 9])))

      (testing "*"
        (= '{*rest [4 5 6 7] ?tag :1 ?2 2 ?3 3 ?8 8 ?9 9}
           (get-bindings '[?tag ?2 ?3 *rest ?8 ?9]
                         [:1 2 3 4 5 6 7 8 9]))
        (= '{?tag :1 ?2 2 ?3 3 ?8 8 ?9 9}
           (get-bindings '[?tag ?2 ?3 *rest ?8 ?9]
                         [:1 2 3 8 9])))))

  (testing "fragments"
    (testing "with prefixed variables"
      (is (= '{?tag :p ?x 1 ?y 2 <> [{?z 1}]}
             (get-bindings '[?tag ?x ?y [:<> ?z 2]]
                           [:p 1 2 1 2]))))

    (testing "with variables on both sides"
      (is (= '{?tag :p ?x 0 ?y 3 <> [{?z 2}]}
             (get-bindings '[?tag ?x [:<> ?z 1] ?y]
                           [:p 0 2 1 3]))))

    (testing "with quantifiers"
      (testing "non-greedy"
        (is (= '{?tag :p ?x 0 ?y 99 <> [{?n 1 *rest []}
                                        {?n 2 *rest []}
                                        {?n 3 *rest []}
                                        {?n 1 *rest []}
                                        {?n 2 *rest []}
                                        {?n 3 *rest []}]}
               (get-bindings '[?tag ?x [:<> ?n *rest] ?y]
                             [:p 0 1 2 3 1 2 3 99]))))

      (testing "fragments cannot intersect"
        (is (= nil
               (get-bindings '[?tag ?x [:<> ?n +rest] ?y]
                             [:p 0 1 2 3 1 2 3 99]))))

      ;; TODO: more tests, better tests
      (testing "fragments are bounded"
        (is (= '{+a [1] ?b 5 <> [{*y [2 3]}]}
               (get-bindings '[:p +a [:<> *y 4] ?b]
                             '[:p 1 2 3 4 5])))

        ;; No matching after pattern.
        (is (= nil
               (get-bindings '[:p +a [:<> *y 4] ?b]
                             '[:p 1 2 3 4])))

        (is (= '{?b 5 <> [{*y [1 2 3]}]}
               (get-bindings '[:p [:<> *y 4] ?b]
                             '[:p 1 2 3 4 5]))))

      (testing "complex example"
        (is (= '{?type   "letter"
                 +before [[:fw {}]
                          [:opener
                           {:xml:id "xx"}
                           [:dateline {} [:settlement {:ref "#STED"}] [:date {:when "1942-06-30"}]]
                           [:salute {} [:persname {:type "receiver", :ref "xx"} "MODTAGER "]]]]
                 <>      [{?n            "1"
                           ?facs         "24.628"
                           +page-content [[:p {:xml:id "p1"} [:persname {:ref "#np60"} "Jens Holt"] " , 16 "]
                                          [:p {:xml:id "p2"} [:date {:when "1942-06-30"} "30 juni 1942"]]
                                          [:p {:xml:id "p3"} "Kære " [:persname {:ref "#np60"} "Holt"] " ,"]]}
                          {?n            "2"
                           ?facs         "24.629"
                           +page-content [[:p {:xml:id "p5"} "2 "]
                                          [:p {:xml:id "p6"} "mit bord maaned efter maaned et fuldt færdigt manuskript , som ikke kan komme ud !"]]}]}
               (get-bindings '[:div {:type ?type}
                               +before
                               [:<>
                                [:pb {:n ?n :facs ?facs}]
                                +page-content]]

                             [:div {:type "letter"}

                              ;; +before
                              [:fw {}]
                              [:opener
                               {:xml:id "xx"}
                               [:dateline {} [:settlement {:ref "#STED"}] [:date {:when "1942-06-30"}]]
                               [:salute {} [:persname {:type "receiver", :ref "xx"} "MODTAGER "]]]

                              ;; <>, first
                              [:pb {:n "1", :facs "24.628"}]
                              [:p {:xml:id "p1"} [:persname {:ref "#np60"} "Jens Holt"] " , 16 "]
                              [:p {:xml:id "p2"} [:date {:when "1942-06-30"} "30 juni 1942"]]
                              [:p {:xml:id "p3"} "Kære " [:persname {:ref "#np60"} "Holt"] " ,"]

                              ;; <>, second
                              [:pb {:n "2", :facs "24.629"}]
                              [:p {:xml:id "p5"} "2 "]
                              [:p {:xml:id "p6"} "mit bord maaned efter maaned et fuldt færdigt manuskript , som ikke kan komme ud !"]])))))))


(deftest test-matches
  (testing "matches function (based on bindings)"
    (is (= hiccup-example
           (matches from hiccup-example)))))

(deftest test-transform
  (testing "preservation"
    (is (= hiccup-example
           (transform from
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
           (transform from
                      '[?div [?p] [?span {:class ?class}]]
                      hiccup-example))))

  (testing "fn/cuphic transformation"
    (is (= [:div [:p] [:span {:class "class"}]]
           (transform (fn [hiccup] symbol->value)
                      '[?div [?p] [?span {:class ?class}]]
                      hiccup-example))))

  (testing "cuphic/fn transformation"
    (is (= [:div [:p] [:span {:class "class"}]]
           (transform from
                      (fn [{:syms [?div ?p ?span ?class]}]
                        [?div [?p] [?span {:class ?class}]])
                      hiccup-example))))

  (testing "fn/fn transformation"
    (is (= [:div [:p] [:span {:class "class"}]]
           (transform (fn [hiccup] symbol->value)
                      (fn [{:syms [?div ?p ?span ?class]}]
                        [?div [?p] [?span {:class ?class}]])
                      hiccup-example))))

  (testing "fragments"
    (testing "switching pairs"
      (is (= [:p 1 2 3 4 5 6]
             (transform '[:p [:<> ?x ?y]]
                        '[:p [:<> ?y ?x]]
                        '[:p 2 1 4 3 6 5])))

      (is (= [:p 2 1 8 9 8 9]
             (transform '[:p ?x ?y [:<> 1 2]]
                        '[:p ?y ?x [:<> 8 9]]
                        [:p 1 2 1 2 1 2]))))

    ; TODO
    (testing "splicing + interaction with quantifiers"))

  (testing "quantifiers"
    (testing "splicing"
      (= [:p 7 8 9 "-" 1 2 3 "-" 4 5 6]
         (transform '[:p +begin "-" +middle "-" +end]
                    '[:p +end "-" +begin "-" +middle]
                    '[:p 1 2 3 "-" 4 5 6 "-" 7 8 9])))))

(deftest test-scraping
  (testing "scanning a Hiccup tree"
    (is (= '([[?tag {:id ?id}]
              {?tag :p, ?id "a"}
              [[:p {:id "a"} [:span {:id "b"}]] {:l [], :pnodes [[:div {} [:p {:id "a"} [:span {:id "b"}]]]], :ppath nil, :r nil}]]

             [[:span {:id ?id}]
              {?id "b"}
              [[:span {:id "b"}]
               {:l      [],
                :pnodes [[:div {} [:p {:id "a"} [:span {:id "b"}]]] [:p {:id "a"} [:span {:id "b"}]]],
                :ppath  {:l [], :pnodes [[:div {} [:p {:id "a"} [:span {:id "b"}]]]], :ppath nil, :r nil},
                :r      nil}]]

             [[?tag {:id ?id}]
              {?tag :span, ?id "b"}
              [[:span {:id "b"}]
               {:l      [],
                :pnodes [[:div {} [:p {:id "a"} [:span {:id "b"}]]] [:p {:id "a"} [:span {:id "b"}]]],
                :ppath  {:l [], :pnodes [[:div {} [:p {:id "a"} [:span {:id "b"}]]]], :ppath nil, :r nil},
                :r      nil}]])

           (scan [:div {}
                  [:p {:id "a"}
                   [:span {:id "b"}]]]
                 '[?tag {:id "nada"}]
                 '[:span {:id ?id}]
                 '[?tag {:id ?id}]))))

  (testing "scraping a Hiccup tree"
    (let [actual (scrape [:div {}
                          [:p {:id "a"}
                           [:span {:id "b"}]]]
                         '[?tag {:id "nada"}]
                         '[:span {:id ?id}]
                         '[?tag {:id ?id}])]
      (is (= '[nil
               ({?id "b"})
               ({?tag :p, ?id "a"}
                {?tag :span, ?id "b"})]

             actual))

      (testing "scrape includes metadata for the zipper loc"
        (let [[empty-result [x] [y z]] actual
              meta-loc? (comp :zip/branch? meta :loc meta)]
          (is (nil? empty-result))
          (is (not= x y z))
          (is (some? (meta-loc? x)))
          (is (some? (meta-loc? y)))
          (is (some? (meta-loc? z))))))))
