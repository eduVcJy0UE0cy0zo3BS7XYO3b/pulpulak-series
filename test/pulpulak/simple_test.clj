(ns pulpulak.simple-test
  (:require [clojure.test :refer :all]))

(deftest simple-addition-test
  (testing "Basic arithmetic works"
    (is (= 4 (+ 2 2)))
    (is (= 10 (* 5 2)))
    (is (= 3 (- 5 2)))))

(deftest simple-collection-test
  (testing "Basic collection operations work"
    (is (= [1 2 3] (conj [1 2] 3)))
    (is (= {:a 1 :b 2} (assoc {:a 1} :b 2)))
    (is (= 3 (count [1 2 3])))))

(deftest pure-functional-test
  (testing "Pure functional operations"
    (let [add-one (fn [x] (+ x 1))
          multiply-by-two (fn [x] (* x 2))
          compose (fn [f g] (fn [x] (f (g x))))
          add-one-then-double (compose multiply-by-two add-one)]
      (is (= 6 (add-one-then-double 2)))
      (is (= 8 (add-one-then-double 3))))))

(deftest immutability-test
  (testing "Data structures are immutable"
    (let [original-vec [1 2 3]
          new-vec (conj original-vec 4)]
      (is (= [1 2 3] original-vec))
      (is (= [1 2 3 4] new-vec))
      (is (not= original-vec new-vec)))))