(use `clojure.string)

(def super-split-keys [:title-string
                       :artist-string
                       :genre-string
                       :stuf-string])

(def reg #"\{|\}|\[|\]|:and|:not|:or")

(def replace-map {"{" "(line-serch :TOD {"
                  "}" "})"
                  "[" "("
                  "]" ")"
                  ":and" "and"
                  ":not" "not"
                  ":or" "or"})

(defn super-split [xs]
  (for [line (split-lines xs)]
    (zipmap super-split-keys (-> line
                                 trim
                                 (split #"(\s\s+)|(\.\s*)")))))



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
  (-> xs
    super-split
    generate-collection))



(defn line-serch [line criteria]
   (every? identity
     (vec
      (for [c criteria]
       (let [k (get c 0)
             v (get c 1)]
         (cond

          (= k :tag)
          (if (<= 1 (count (filter #{v} (line :tags))))
           true)

          :else
          (= (line k) v)))))))


(defn build [x]
  (->> x
     (filter identity)
     (apply hash-set)))



(defn eval-str [criteria x]
  (-> (replace (replace (str criteria) reg replace-map) #":TOD" (str x))
      read-string
      eval))


(defn search [collection serch-for criteria]
   (build
     (if (= serch-for :tags)
       (apply concat
              (for [x collection]
                (when (eval-str criteria x)
                  (if (= serch-for :tags)
                    (for [i (x :tags)] i)
                    (x serch-for)))))
       (for [x collection]
         (when (eval-str criteria x)
           (if (= serch-for :tags)
             (for [i (x :tags)] i)
             (x serch-for)))))))


(use 'clojure.test)

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


