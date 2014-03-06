

(defn index-by [f x]
  (zipmap
   (vec
    (map
      #(f %) x))
   (vec x)))

(index-by count ["a" "ba"])