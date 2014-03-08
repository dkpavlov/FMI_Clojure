(use 'clojure.test)

(defn uniquify [xs]
  (for [i (range 1 (inc (count xs)))
        :let [s (get xs (dec i))
              subxs (subvec xs 0 i)]]
    (cond
      (= 1 (count (filter #{s} subxs))) s
      :else (str s "-" (dec (count (filter #{s} subxs)))))))

(uniquify ["a" "b" "c" "a"])

(deftest test-uniquify
  (is (= (uniquify []) []))
  (is (= (uniquify ["a"]) ["a"]))
  (is (= (uniquify ["a" "a"]) ["a" "a-1"]))
  (is (= (uniquify ["a" "b" "a"]) ["a" "b" "a-1"]))
  (is (= (uniquify ["a" "a" "b" "c" "b" "a"]) ["a" "a-1" "b" "c" "b-1" "a-2"]))
  (is (= (uniquify ["ala" "ala" "ala" "ala"]) ["ala" "ala-1" "ala-2" "ala-3"])))


(run-tests)
