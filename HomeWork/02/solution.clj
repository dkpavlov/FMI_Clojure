
(def tt (str [:and {:genre "Jazz"} [:not {:title "My Favourite Things"}] [:or {:tag :popular} {:tag :bebop}]]))

(read-string
 (clojure.string/replace tt #"\{|\}|\[|\]|:and|:not|:or" {"{" "(line-serch x {" "}" "})" "[" "(" "]" ")" ":and" "and" ":not" "not" ":or" "or"}))

(use `clojure.string)

(def test1 [:stuf-string :genre-string :artist-string :title-string])
(def test2 [:title-string :artist-string :genre-string :stuf-string])


(defn super-split [xs]
  (for [line (split-lines xs)]
    (zipmap test2 (split (trim line) #"(\s\s+)|(0\s*)|(\.\s*)"))))


(super-split "My Favourite Things.          John Coltrane.      Jazz, Bebop.        popular, cover
              Alabama.                      John Coltrane.      Jazz, Avantgarde.
              Boplicity.                    Miles Davis.        Jazz, Bebop
              Autumn Leaves.                Bill Evans.         Jazz.               popular
              Waltz for Debbie.             Bill Evans.         Jazz
              Pathetique.                   Beethoven.          Classical
              Fur Elise.                    Beethoven.          Classical.          popular
              Toccata e Fuga.               Bach.               Classical, Baroque.
              Eine Kleine Nachtmusik.       Mozart.             Classical.          violin, fancy")

(defn make-collection [super-list]
  (for [x super-list]
    {:title (x :title-string)
     :artist (x :artist-string)
     :genre (if (not= (.indexOf (x :genre-string) ", ") -1)
              (get (split (x :genre-string) #",\s") 0)
              (x :genre-string))
     :subgenre (if (not= (.indexOf (x :genre-string) ", ") -1)
                 (get (split (x :genre-string) #",\s") 1)
                 nil)
     :tags (vec
            (if (or (not (nil? (x :stuf-string))) (not (nil? (x :genre-string))))
             (for [xs (split (str (x :genre-string) ", " (x :stuf-string)) #",\s")]
               (keyword (lower-case xs)))))}))



(def testt (make-collection (super-split "My Favourite Things.          John Coltrane.      Jazz, Bebop.        popular, cover
                              Alabama.                      John Coltrane.      Jazz, Avantgarde.
                              Boplicity.                    Miles Davis.        Jazz, Bebop
                              Autumn Leaves.                Bill Evans.         Jazz.               popular
                              Waltz for Debbie.             Bill Evans.         Jazz
                              Pathetique.                   Beethoven.          Classical
                              Fur Elise.                    Beethoven.          Classical.          popular
                              Toccata e Fuga.               Bach.               Classical, Baroque.
                              Eine Kleine Nachtmusik.       Mozart.             Classical.          violin, fancy")))

(make-collection (super-split "My Favourite Things.          John Coltrane.      Jazz, Bebop.        popular, cover
                              Alabama.                      John Coltrane.      Jazz, Avantgarde.
                              Boplicity.                    Miles Davis.        Jazz, Bebop
                              Autumn Leaves.                Bill Evans.         Jazz.               popular
                              Waltz for Debbie.             Bill Evans.         Jazz
                              Pathetique.                   Beethoven.          Classical
                              Fur Elise.                    Beethoven.          Classical.          popular
                              Toccata e Fuga.               Bach.               Classical, Baroque.
                              Eine Kleine Nachtmusik.       Mozart.             Classical.          violin, fancy"))




(def reg #"\{|\}|\[|\]|:and|:not|:or")
(def replace-map {"{" "(line-serch :TOD {" "}" "})" "[" "(" "]" ")" ":and" "and" ":not" "not" ":or" "or"})
(def test-string (str [:and {:genre "Jazz"} [:not {:title "My Favourite Things"}]]))

(read-string (clojure.string/replace test-string reg replace-map))


(defn line-serch [line criteria]
   (every? identity
     (vec (for [c criteria]
       (cond
        (= (get c 0) :tag)
         (if (<= 1 (count (filter #{(get c 1)} (line :tags))))
           true)
        :else (= (line (get c 0)) (get c 1)))))))

;TODO da se wzimat tagowete
(defn search [collection serch-for criteria]
   (apply hash-set
          (filter identity
                  (if (= serch-for :tags)
                    (apply concat (for [x collection]
                                  (when (eval (read-string (clojure.string/replace (clojure.string/replace (str criteria) reg replace-map) #":TOD" (str x))))
                                    (if (= serch-for :tags)
                                      (for [i (x :tags)] i)
                                      (x serch-for)))))
                    (for [x collection]
                                  (when (eval (read-string (clojure.string/replace (clojure.string/replace (str criteria) reg replace-map) #":TOD" (str x))))
                                    (if (= serch-for :tags)
                                      (for [i (x :tags)] i)
                                      (x serch-for)))))
                  )))

;МАИН ТЕСТ
(search testt :title [:and {:genre "Jazz"} [:not {:title "My Favourite Things"}] [:or {:tag :popular} {:tag :bebop}]])

(hash-set [nil nil "Boplicity" "Autumn Leaves" nil nil nil nil nil])

(filter identity [nil nil "Boplicity" "Autumn Leaves" nil nil nil nil nil])


(use 'clojure.test)

(load-file "solution.clj")

(def songs-text2
"My Favourite Things. John Coltrane. Jazz, Bebop. popular, cover
Alabama. John Coltrane. Jazz, Avantgarde.
Boplicity. Miles Davis. Jazz, Bebop
Autumn Leaves. Bill Evans. Jazz. popular
Waltz for Debbie. Bill Evans. Jazz
Pathetique. Beethoven. Classical
Fur Elise. Beethoven. Classical. popular
Toccata e Fuga. Bach. Classical, Baroque.
Eine Kleine Nachtmusik. Mozart. Classical. violin, fancy")

(deftest task-02-sample-test
  (def collection (make-collection (super-split songs-text2)))
  (is (= (search collection :artist {:genre "Jazz"})
         #{"John Coltrane" "Miles Davis" "Bill Evans"}))
  (is (= (search collection :subgenre {:artist "John Coltrane"})
         #{"Bebop" "Avantgarde"}))
  (is (= (search collection :tags {:artist "John Coltrane"})
         #{:jazz :bebop :popular :cover :avantgarde}))
  (is (= (search collection :title [:and {:genre "Classical"} {:tag :violin}])
         #{"Eine Kleine Nachtmusik"}))
  (is (= (search collection :title [:and {:genre "Jazz"}
                                         [:not {:title "My Favourite Things"}]
                                         [:or {:tag :popular} {:tag :bebop}]])
         #{"Autumn Leaves" "Boplicity"})))

(run-tests)

(search collection :tags {:artist "John Coltrane"})

(concat  `(:a :b) `(:a :b) `(:c :D))

(search collection :subgenre {:artist "John Coltrane"})

(apply concat (for [x testt]
  (for [i (x :tags)] i)))





