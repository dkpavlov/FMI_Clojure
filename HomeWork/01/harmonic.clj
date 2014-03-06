(defn harmonic [n]
  (apply + (for
             [x (range 1 (+ 1 n))] (/ 1 x))))