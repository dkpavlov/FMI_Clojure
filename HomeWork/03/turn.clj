(def moves {:left [-1 0] :top [0 1] :right [1 0] :bottom [0 -1]})

(defn sub-turn [old new]
  (if (= (map + new old) [0 0])
    old
    new))

(defn turn [snake o]
  (let [old (snake :dir)]
    (->> (moves o)
         (sub-turn old)
         (assoc snake :dir))))

(def sub-turn [old new]
  (if (= (map + new old) [0 0])
    old
    new))

(def snake {:dir [0 1]
              :width 10
              :height 10
              :prizes #{[3 8] [1 8]}
              :location [[3 3] [3 4] [3 5] [3 6]]})

(turn snake :bottom)

