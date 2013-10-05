(ns parser.core
  (require [clojure.java.jdbc :as jdbc]
           [clojure.java.io :as io])
  (import [org.jfree.data.statistics SimpleHistogramDataset]
          [org.jfree.data.statistics SimpleHistogramBin]
          [org.jfree.chart ChartFactory]
          [org.jfree.chart.plot PlotOrientation]
          [org.jfree.chart.axis LogarithmicAxis]
))

(defn write [vals filename & headers]
  (with-open [wrt (io/writer filename)]
    (.write wrt (str (clojure.string/join "," (map name headers)) "\n"))
    (doseq [val vals]
      (.write wrt (str (clojure.string/join "," (map #(or (get val %) 0) headers)) "\n")))))

(defn make-histogram [m]
  (let [h (new SimpleHistogramDataset "Dataset")]
    (doseq [[key val] m]
      (let [b (new SimpleHistogramBin key (+ 1 key) true false)]
        (.setItemCount b val)
        (.addBin h b)))
    (let [chart (ChartFactory/createHistogram "Simple Histogram Demo 1", nil, nil, h, PlotOrientation/VERTICAL, true, true, false)
          axis (new LogarithmicAxis "Number of things")]
      (.setRangeAxis (.getPlot chart) axis)
      chart)))

(defn lazy-query [db-uri query]
  (let [c (jdbc/get-connection db-uri)
        a (.setAutoCommit c false)
        s (.createStatement c)
        v (.setFetchSize s 2000)
        rs (.executeQuery s query)]
    (jdbc/result-set-seq rs)))

(defn qq [query]
  (lazy-query "jdbc:postgresql://localhost/postgres" query))

(def i (atom 0))

(def link-regex #"(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)*\/?")
(def issue-regex #"issue:?=?\s*#?\s*(\d\d\d+)(.*)")
(def bug-regex #"bugi?d?:?=?\s*#?\s*(\d\d\d+)(.*)")
(def email-regex #"([a-z0-9_\.-]+)@([\da-z\.-]+)\.([a-z\.]{2,6})")


(defn count-regex  [query extractor]
  (loop [rs (qq query)
         cnt 0
         i 0]
    (if (= 0 (mod i 100000)) (println i))
    (let [next (first rs)
          item (extractor next)]
      (if next
        (recur
         (rest rs)
         (+ cnt
            (count (re-seq issue-regex item)) 
            (count (re-seq bug-regex item)))
         (+ 1 i))
        cnt))))

(defn count-email  [query extractor]
  (loop [rs (qq query)
         cnt 0
         i 0]
    (if (= 0 (mod i 100000)) (println i))
    (let [next (first rs)
          item (extractor next)]
      (if next
        (recur
         (rest rs)
         (+ cnt
            (count (re-seq email-regex item)))
         (+ 1 i))
        cnt))))

(defn mreduce [pairs list]
  (let [fns (map first pairs)
        inits (map second pairs)]
    (reduce 
     (fn [accum val]
       (doall (map (fn [f a] (f a val)) fns accum)) )
     inits
     list)))

(defn count-regexes [& regexes]
  (fn [accum val]
    (+ accum (apply + (doall (map #(count (re-seq % val)) regexes))))))

(defn count-reg [query extractor]
  (mreduce [[(count-regexes email-regex) 0]]
           (map extractor (qq query))))

(defn reduce-email [accum val]
  (+ accum (count (re-seq email-regex val))))

(defn reduce-bug [accum val]
  (+ accum (count (re-seq email-regex val))))

(defn count-map [reducer kfn vfn]
  (fn [accum val]
    (update-in accum [(kfn val)] 
               (fn [a] (reducer a (vfn val))))))

(defn all-comments [query extractor]
  (mreduce [[(count-regexes email-regex) 0]
            [(count-regexes issue-regex bug-regex) 0]
            [(count-regexes link-regex) 0]]
           (map extractor (qq query))))


(defn questions-file []
  (write (qq "select score, comment_count, favorite_count, answer_count, view_count from posts where post_type_id=1") "questions.csv" :score :comment_count :favorite_count :answer_count :view_count))

(defn answers-file []
  (write (qq "select score, comment_count from posts where post_type_id=2") "answers.csv" :score :comment_count))

(defn user-file []
  (write (qq "select views, up_votes, down_votes from users") "users.csv" :views :up_votes :down_votes))

(defn comments-file []
  (write (qq "select score from comments") "comments.csv" :score))

(defn all-stats [query kfn vfn]
  (mreduce [[(count-map (count-regexes email-regex) kfn vfn) 
             {nil 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0}]
      
      [(count-map (count-regexes issue-regex bug-regex) kfn vfn) 
             {nil 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0}]
            [(count-map (count-regexes link-regex) kfn vfn) 
             {nil 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0}]]
           (qq query)))

(defn hist [vals]
  (reduce (fn [accum val] (update-in accum [(or val 0)] #(+ 1 (or % 0)))) 
          {}
          vals))


(def comment-trace (list {nil 0, 1 2675, 2 5680, 3 0, 4 0, 5 0, 6 0, 7 0} {nil 0, 1 285, 2 539, 3 0, 4 0, 5 0, 6 0, 7 0} {nil 0, 1 1241586, 2 2128905, 3 1, 4 0, 5 0, 6 282, 7 0})
)

(def post-trace (list {1 49748, 2 30847, 3 0, 4 0, 5 19, 6 1, 7 0} {1 608, 2 2321, 3 0, 4 0, 5 1, 6 0, 7 0} {1 12130779, 2 12252559, 3 92, 4 728, 5 21194, 6 272, 7 9}))



(def question-answer-count {0 299952, 32 43, 64 3, 320 1, 1 1389618, 33 41, 65 5, 97 1, 129 2, 2 873715, 34 39, 66 2, 3 444096, 35 40, 67 1, 99 2, 131 2, 163 1, 195 1, 4 215944, 36 20, 68 2, 100 4, 5 105626, 37 28, 69 2, 101 1, 6 52791, 38 16, 70 1, 134 1, 7 27515, 39 20, 71 2, 519 1, 8 15356, 40 21, 72 4, 136 1, 296 1, 9 8987, 41 11, 73 3, 105 1, 10 5500, 42 9, 74 3, 106 1, 11 3734, 43 15, 107 1, 12 2555, 44 12, 13 1809, 45 10, 14 1319, 46 14, 78 5, 15 1008, 47 5, 79 1, 16 717, 48 7, 80 2, 112 2, 176 1, 208 1, 17 575, 49 5, 81 1, 18 446, 50 8, 114 1, 19 360, 51 7, 83 1, 20 296, 52 8, 84 2, 21 244, 53 5, 85 1, 22 222, 54 4, 150 1, 214 1, 23 162, 55 8, 87 3, 24 142, 56 6, 25 111, 57 2, 89 1, 26 101, 58 3, 90 1, 27 88, 59 3, 91 1, 123 2, 28 74, 60 2, 92 1, 316 1, 29 62, 61 4, 93 1, 30 60, 31 55, 63 3, 191 1, 415 1})
