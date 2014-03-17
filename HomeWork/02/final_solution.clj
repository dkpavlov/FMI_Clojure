(use `clojure.string)

(def super-split-keys [:title-string :artist-string :genre-string :stuf-string])

(def reg #"\{|\}|\[|\]|:and|:not|:or")

(def replace-map {"{" "(line-serch :TOD {" "}" "})" "[" "(" "]" ")" ":and" "and" ":not" "not" ":or" "or"})

(defn super-split [xs]
  (for [line (split-lines xs)]
    (zipmap super-split-keys (split (trim line) #"(\s\s+)|(0\s*)|(\.\s*)"))))

(defn generate-collection [super-list]
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

(defn make-collection [xs]
  (generate-collection (super-split xs)))

(defn line-serch [line criteria]
   (every? identity
     (vec (for [c criteria]
       (cond
        (= (get c 0) :tag)
         (if (<= 1 (count (filter #{(get c 1)} (line :tags))))
           true)
        :else (= (line (get c 0)) (get c 1)))))))


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
                                      (x serch-for))))))))





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
  (def collection (make-collection songs-text2))
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


