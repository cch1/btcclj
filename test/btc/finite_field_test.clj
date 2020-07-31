(ns btc.finite-field-test
  (:require [btc.finite-field :refer :all]
            [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(deftest operations
  (is (= 0  (+f 19)))
  (is (= 10 (+f 19 10)))
  (is (= 3  (+f 19 14 8)))
  (is (= 12 (+f 19 16 3 12)))
  (is (= 7  (-f 19 12)))
  (is (= 15 (-f 19 12 16)))
  (is (= 0  (-f 19 2 8 13)))
  (is (= 1  (*f 19)))
  (is (= 14 (*f 19 14)))
  (is (= 9  (*f 19 14 2)))
  (is (= 16 (*f 19 6 2 14)))
  (is (= 11 (divf 19 7)))
  (is (= 3  (divf 19 2 7)))
  (is (= 4  (**f 53 9 23)))
  (is (= 1  (**f 23 5 0))))

;; This solution is tail-recursive and passes a fully realized data structure that
;; grows s l o w l y.
;; https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;; http://diditwith.net/2009/01/20/YAPESProblemSevenPart2.aspx
;; http://yonatankoren.com/post/3-lazy-prime-sequence
(def primes
  "An infinite, lazy sequence of prime numbers"
  (letfn [(reinsert [table x prime] (update-in table [(+ prime x)] conj prime))]
    ((fn sieve [table d]
       (if-let [factors (get table d)]
         (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                (inc d)) ; tail recursion w/ fully realized data structure
         (lazy-seq (cons d (sieve (assoc table (* d d) (list d))
                                  (inc d)))))) ; lazy seq with fully realized data structure
     {} 2)))

(def prime-field-order (gen/fmap (partial nth primes) (gen/choose 0 10000)))

(def field-element (gen/let [p prime-field-order
                             x (gen/choose 0 p)] [p x]))

(defn field-elements
  "Return a generator of a tuple of the order of a prime field and vector of `n` elements from that field"
  [n]
  (gen/bind prime-field-order ; primes
            (fn [order] (gen/tuple (gen/return order) (gen/vector (gen/choose 0 order) n)))))

(defspec closed-over-operations 10
  (prop/for-all [[p [m n & others]] (field-elements 2)
                 k gen/pos-int]
                (and
                 (element? p (+f p m n))
                 (element? p (-f p m n))
                 (element? p (*f p m n))
                 (element? p (divf p m n))
                 (element? p (**f p m k)))))

(defspec additive-identity 10
  (prop/for-all [[p m] field-element]
                (= m (+f p m 0))))

(defspec multiplicative-identity 10
  (prop/for-all [[p m] field-element]
                (= m (*f p m 1))))

(defspec additive-inverse 10
  (prop/for-all [[p m] field-element]
                (= 0 (+f p m (-f p m)))))

(defspec multiplicative-inverse 10
  (prop/for-all [[p m] field-element]
                (= 1 (*f p m (divf p m)))))
