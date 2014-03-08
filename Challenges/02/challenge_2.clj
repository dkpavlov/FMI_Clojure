(defn fight [first last]
  (let [board {:rock {:scissors :crushes :lizard :crushes}
              :paper {:spock :disproves :rock :covers}
              :scissors {:lizard :decapitate :paper :cut}
              :lizard {:paper :eats :spock :poisons}
              :spock {:rock :vaporizes :scissors :smashes}}]
    (cond
      (not (nil? (get (get board first) last)))
        (vec [first (get (get board first) last) last])
      :else
         (vec [last (get (get board last) first) first]))))

(fight :lizard :scissors)
(fight :scissors :spock)
(fight :lizard :paper)

(use 'clojure.test)

(deftest challenge-02-sample-test
  (is (= (fight :scissors :spock) [:spock :smashes :scissors])))

(run-tests)
