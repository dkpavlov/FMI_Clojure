(defn form-digits [numbers]
  (read-string
   (apply str numbers)))


(defn digits [number]
  seq (str number))


(form-digits [1 2 3])


(digits)

(defn prime-factors
  ([n]
    (prime-factors n 2 '()))
  ([n candidate acc]
    (cond (<= n 1) (vec (reverse acc))
          (zero? (rem n candidate)) (recur (/ n candidate) candidate (cons candidate acc))
          :else (recur n (inc candidate) acc))))
