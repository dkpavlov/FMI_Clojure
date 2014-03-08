(ns exploring.rock-paper-scissors
  (:refer-clojure :exclude (==))
  (:use [clojure.core.logic]))

(defrel rps winner defeats loser)

(fact rps :scissors :cut :paper)
(fact rps :paper :covers :rock)
(fact rps :rock :crushes :lizard)
(fact rps :lizard :poisons :spock)
(fact rps :spock :melts :scissors)
(fact rps :scissors :decapitate :lizard)
(fact rps :lizard :eats :paper)
(fact rps :paper :disproves :spock)
(fact rps :spock :vaporizes :rock)
(fact rps :rock :breaks :scissors)

(defrel rank title person)
(fact rank :captain :kirk)
(fact rank :commander :spock)
(fact rank :lieutenant :uhura)
(fact rank :ensign :chekov)

;; how can I defeat paper?
(run* [verb]
      (fresh [winner]
             (rps winner verb :paper)))

;; what can kill?
(run* [winner]
      (fresh [verb loser]
             (rps winner verb loser)))

;; find me one thing that can kill a Star Trek officer
(run 1 [winner]
     (fresh [verb loser title]
            (rps winner verb loser)
            (rank title loser)))

;; functional approach (for comparison)
(def rps-facts
  [[ :scissors :cut :paper ]
   [ :paper :covers :rock ]
   [ :rock :crushes :lizard ]
   [ :lizard :poisons :spock ]
   [ :spock :melts :scissors ]
   [ :scissors :decapitate :lizard ]
   [ :lizard :eats :paper ]
   [ :paper :disproves :spock ]
   [ :spock :vaporizes :rock ]
   [ :rock :breaks :scissors ]])

(defn wins
  [verb loser]
  (->> rps-facts
       (filter (fn [[_ v l]]
                 (and (= verb v) (= l loser))))
       (mapv first)))

(wins :vaporizes :rock)