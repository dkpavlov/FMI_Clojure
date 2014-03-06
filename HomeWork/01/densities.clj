(defn densities [s]
  (for [x s]
    (count
      (filter #{x} s))))
