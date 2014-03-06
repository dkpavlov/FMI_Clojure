; Local problem with name digits for function

(defn d [number]
   (vec (map #(Character/digit % 10) (str number))))

(= (d 22) [2 2])