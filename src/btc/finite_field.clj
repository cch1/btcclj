(ns btc.finite-field
  "Follow along for https://btcclj.com/, but with clarity and idiomatic Clojure")

(defn- modpow [base exponent modulus]
  (loop [result 1 exponent exponent]
    (if (zero? exponent)
      result
      (recur (mod (* result base) modulus) (dec exponent)))))

(defn- fermat-prime?
  "Does the given integer `p` pass the Fermat test for primality against the given `a`?"
  ;; NB: This test does not distinguish between primes and Charmichael numbers, or psuedoprimes as a consequence of using FLT
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

(defn element?
  "Is the given element `element` a member of the finite field of order `p`?"
  [p x]
  {:pre [(integer? p) (integer? x)]}
  (and (<= 0 x) (< x p)))

;; Define operations applicable to _prime_ fields
(defn +f
  ([p] 0) ; nullary returns additive identity
  ([p n] (+f p 0 n))
  ([p n m] {:pre [(element? p n) (element? p m) (prime? p)]} (mod (+ n m) p))
  ([p n m & ms] (reduce (partial +f p) (+f p n m) ms)))

(defn -f
  ([p n] (-f p 0 n)) ; unary returns the additive inverse of n
  ([p n m] {:pre [(element? p n) (element? p m) (prime? p)]} (mod (- n m) p))
  ([p n m & ms] (reduce (partial -f p) (-f p n m) ms)))

(defn *f
  ([p] 1) ; nullary returns multiplicative identity
  ([p n]  (*f p 1 n))
  ([p n m] {:pre [(element? p n) (element? p m) (prime? p)]} (mod (* n m) p))
  ([p n m & ms] (reduce (partial *f p) (*f p n m) ms)))

(defn divf ; ⧸ ∕
  ([p n] (divf p 1 n)) ; unary returns the multiplicative inverse of n
  ([p n m] {:pre [(element? p n) (element? p m) (prime? p)]}
   (let [m' (modpow m (- p 2) p)] ; from Fermat's Little Theorem
     (*f p n m')))
  ([p n m & ms] (reduce (partial divf p) (divf p n m) ms)))

(defn **f [p n k] {:pre [(element? p n) (prime? p)]} (modpow n k p))
