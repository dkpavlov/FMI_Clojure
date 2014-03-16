
(def tt (str [:and {:genre "Jazz"} [:not {:title "My Favourite Things"}] [:or {:tag :popular} {:tag :bebop}]]))

(read-string
 (clojure.string/replace tt #"\{|\}|\[|\]|:and|:not|:or" {"{" "(test l {" "}" "})" "[" "(" "]" ")" ":and" "and" ":not" "not" ":or" "or"}))

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
     :tags (if (not (nil? (x :stuf-string)))
             (for [xs (split (str (x :genre-string) ", " (x :stuf-string)) #",\s")]
               (lower-case xs)))}))



(make-collection (super-split "My Favourite Things.          John Coltrane.      Jazz, Bebop.        popular, cover
                              Alabama.                      John Coltrane.      Jazz, Avantgarde.
                              Boplicity.                    Miles Davis.        Jazz, Bebop
                              Autumn Leaves.                Bill Evans.         Jazz.               popular
                              Waltz for Debbie.             Bill Evans.         Jazz
                              Pathetique.                   Beethoven.          Classical
                              Fur Elise.                    Beethoven.          Classical.          popular
                              Toccata e Fuga.               Bach.               Classical, Baroque.
                              Eine Kleine Nachtmusik.       Mozart.             Classical.          violin, fancy"))

(defn search [collection serch-for criteria]
  (for [x collection]
    (if (= (x )))))

(zipmap [:a :b :c] ["a" "b"])

(.indexOf "a, b" ", ")

(split "ab" #",\s")



