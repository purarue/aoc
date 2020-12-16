(require '[clojure.string :as str])
(defn parse-input [input-file]
  (with-open [rdr (clojure.java.io/reader input-file)]
    (map bigint
         (map #(Integer/parseInt %)
              (str/split
               (str/trim (first (line-seq rdr)))
               #",")))))

(defn compute-score [last-arr] (- (nth last-arr 0) (nth last-arr 1)))

(defn add-to-hash-link [pspoken last-spoken turn]
  (if (contains? pspoken last-spoken)
    (assoc pspoken last-spoken (take 2 (cons turn (pspoken last-spoken))))
    (assoc pspoken last-spoken (cons turn '()))))

(defn findresult [spoken cur turn target]
  (let [next-num (if (and (contains? spoken cur) (>= (count (spoken cur)) 2))
                   (compute-score (spoken cur))
                   0)
        nspoken (add-to-hash-link spoken next-num turn)]
    (if (= turn target)
      next-num
      (recur nspoken next-num (+ 1 turn) target))))

(defn enum-indices [from lst]
  (map vector lst (map list (drop from (range)))))

(defn part [start-map last-num input-count target-num]
  (findresult start-map (bigint last-num) (bigint (+ 1 input-count)) (bigint target-num)))

(defn main [input-file]
  (let [input, (parse-input input-file)
        start-map (apply hash-map (apply concat (enum-indices 1 input)))]
    (println "Part 1:" (part start-map (last input) (count input) 2020))
    (println "Part 2:" (part start-map (last input) (count input) 30000000))))

(main (first *command-line-args*))
