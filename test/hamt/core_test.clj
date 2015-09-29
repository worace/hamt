(ns hamt.core-test
  (:require [clojure.test :refer :all]
            [hamt.core :refer :all]))

(deftest test-new-trie
  (testing "hamt gives empty trie"
    (is (nil? (:key (hamt))))
    (is (nil? (:value (hamt))))))

(deftest test-assoc-empty-trie
  (testing "adding data to empty trie"
    (is (= :pizza (:key (h-assoc (hamt) :pizza :pie))))))

(deftest test-assoc-positions
  (testing "stickes em in right index"
    (let [t (-> (hamt)
                (h-assoc :pizza :pie)
                (h-assoc :calzone :pocket))]
      (is (= :calzone (get-in t [:children 21 :key])))
      (is (= :pocket (get-in t [:children 21 :value]))))))

(deftest test-assoc-second-value
  (testing "nests deeper in the trie"
    (let [t (-> (hamt)
                (h-assoc :pizza :pie)
                (h-assoc :calzone :pocket))]
      (is (= :pocket (h-get t :calzone))))))


(deftest test-assoc-many-things
  (testing "make it break"
    (let [t (-> (hamt)
                (h-assoc :pizza :pie)
                (h-assoc :calzone :pocket)
                (h-assoc :stromboli :roll)
                (h-assoc :linguine :strings)
                (h-assoc :lasagna :ruffles))]
      (is (= :strings (h-get t :linguine))))))

(deftest test-appending-lower-levels
  (testing "uses next 5 bits of hash code"
    (let [t (-> (hamt)
                (h-assoc :pizza :pie)
                (h-assoc :calzone :pocket)
                ;; 23 has same first 5 bits as calzone
                ;; and different second 5
                (h-assoc 23 "random"))]
      (is (= 23 (get-in t [:children 21 :children 6 :key]))))))
