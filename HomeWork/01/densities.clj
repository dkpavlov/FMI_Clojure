(defn densities [s]
  (vec
  (for [x s]
    (count
      (filter #{x} s)))))