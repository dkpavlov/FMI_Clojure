(defn betwin [x interval]
  (and
   (> x (first interval))
   (< x (last interval))))

(defn danger? [snake]
  (let [next-step (map + (snake :dir) (last (snake :location)))
        next-next-step (map + (snake :dir) next-step)
        board [[0 (snake :width)] [0 (snake :height)]]]
    (->> (map betwin next-step board)
         (every? true?)
         not)))
