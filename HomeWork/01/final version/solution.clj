(defn digits [n]
  (map #(Character/digit % 10) (str n)))

(defn prime-factors
  ([n]
    (prime-factors n 2 '()))
  ([n candidate acc]
    (cond (<= n 1) (vec (reverse acc))
          (zero? (rem n candidate)) (recur (/ n candidate) candidate (cons candidate acc))
          :else (recur n (inc candidate) acc))))

(defn fizzbuzz [n]
  (for [x (range 1 (inc n))]
    (cond
     (zero? (mod x 15)) "fizzbuzz"
     (zero? (mod x 3)) "fizz"
     (zero? (mod x 5)) "buzz"
     :else x)))

(defn densities [xs]
  (for [x xs]
    (count
      (filter #{x} xs))))

(defn index-by [f xs]
  (zipmap
   (vec
    (map
      #(f %) xs))
      xs))

(defn harmonic [n]
  (apply + (for
             [x (range 1 (+ 1 n))] (/ 1 x))))

(defn uniquify [in]
  nil)


(use `clojure.test)

(load-file "solution.clj")

(deftest task-01-sample-test
  (is (= (digits 1024) [1 0 2 4]))
  (is (= (prime-factors 6) [2 3]))
  (is (= (fizzbuzz 7) [1 2 "fizz" 4 "buzz" "fizz" 7]))
  (is (= (densities [:a :a :b :b :c :a]) [3 3 2 2 1 3]))
  (is (= (index-by count ["mu" "foo" "larodi"])
         {2 "mu", 3 "foo", 6 "larodi"}))
  (is (= (harmonic 2) 3/2))
  (is (= (uniquify ["a" "b" "a" "b" "a"])
         ["a" "b" "a-1" "b-1" "a-2" ])))

(run-tests)