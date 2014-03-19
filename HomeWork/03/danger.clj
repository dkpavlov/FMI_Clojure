(defn danger? [snake]
  (let [next-step (map + (snake :dir) (last (snake :location)))
        borders [(dec (snake :width))
                 (dec (snake :height))]]
    (apply or (concat (map > next-step max-step) (map < next-step [0 0]))) ))


(danger? {:dir [0 1]
                   :width  10
                   :height 10
                   :prizes #{[3 7]}
                   :location [[3 5] [3 6] [3 7] [3 8] [3 9]]})