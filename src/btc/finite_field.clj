(ns btc.finite-field
  "Follow along for https://btcclj.com/")

(defrecord FieldElement [e p])

(defn- modpow [base exponent modulus]
  (loop [result 1 exponent exponent]
    (if (zero? exponent)
      result
      (recur (mod (* result base) modulus) (dec exponent)))))

(defn- fermat-prime?
  "Does the given integer `p` pass the Fermat test for primality against the given `a`?"
  ;; NB: This test does not distinguish between primes and Charmichael numbers, or psuedoprimes.
  [p a]
  {:pre [(<= 0 a (dec p))]}
  (= (modpow a p p) a))

(defn- rand-ints
  "Return set of order l of random integers between zero (inclusive) and n (exclusive)"
  [n l]
  {:pre [(<= l n)]}
  ;; NB: I think the worst-case performance of this algorithm approaches O(n²) when n ≈ l.
  (loop [result #{}]
    (if (= (count result) l)
      result
      (recur (conj result (rand-int n))))))

(defn- and-as-fn
  ([] true)
  ([acc] acc)
  ([acc el] (and acc el)))

(defn prime?
  "Is the given integer `p` (to a reasonable degree of certainty) a prime?"
  [p]
  (let [as (rand-ints p (min p 50))]
    (transduce (map (partial fermat-prime? p)) and-as-fn as)))

(defn field-element [e p]
  {:pre [(<= 0 e) (< e p) (prime? p)]}
  (->FieldElement e p))

(defprotocol FieldOperations
  (+f [x y])
  (-f [x y])
  (*f [x y])
  (divf  [x y])
  (**f [x k]))

(defn assert-same-prime-order [{e0 :e p0 :p :as fe0} {e1 :e p1 :p :as fe1}]
  (assert (= p0 p1) "Field elementss need to be of the same prime order"))

(extend-type FieldElement
  FieldOperations
  (+f [{e0 :e p0 :p :as fe0} {e1 :e p1 :p :as fe1}]
    (assert-same-prime-order fe0 fe1)
    (field-element (mod (+ e0 e1) p0) p0))
  (-f [{e0 :e p0 :p :as fe0} {e1 :e p1 :p :as fe1}]
    (assert-same-prime-order fe0 fe1)
    (field-element (mod (- e0 e1) p0) p0))
  (*f [{e0 :e p0 :p :as fe0} {e1 :e p1 :p :as fe1}]
    (assert-same-prime-order fe0 fe1)
    (field-element (mod (* e0 e1) p0) p0))
  (divf [fe0 {e1 :e p1 :p :as fe1}]
    (let [inverse-e1 (modpow e1 (- p1 2) p1) ; from Fermat's Little Theorem
          inverse-fe1 (field-element inverse-e1 p1)]
      (*f fe0 inverse-fe1)))
  (**f [{e :e p :p :as fe} k]
    (field-element (modpow e k p) p)))
