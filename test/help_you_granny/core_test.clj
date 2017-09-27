(ns help-you-granny.core-test
  (:require [clojure.test :refer :all]
            [help-you-granny.core :refer :all]))

(deftest a-test1
  (testing "When all friends are in different towns"
    (def friends ["A1" "A2" "A3" "A4" "A5"])
    (def fTowns  [["A1" "X1"] ["A2" "X2"] ["A3" "X3"] ["A4" "X4"]])
    (def distTble [["X1" 100.0] ["X2" 200.0] ["X3" 250.0] ["X4" 300.0]])
    (is (= (tour friends fTowns distTble) 889))))

