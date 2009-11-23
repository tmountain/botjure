(use 'clojure.contrib.str-utils)

(defn shorten-if [row len]
  (subs row
    0 (if (> (count row) len)
        len
        (count row))))

(defn lengthen-if [row len]
  (if (< (count row) len)
    (str row (str-join "" (for [x (range (- len (count row)))] " ")))
    row))

(defn output-format [fmt table]
  (let [lengths (map count (re-split #" " fmt))]
    (for [row (range (count table))]
      (for [i (range 0 (dec (count (nth row table))))]
        (lengthen-if (shorten-if (nth i (nth row table)) (nth i lengths)) (nth i lengths))))))
      
    
    
[["Florida" "Stupid"]
 ["Georgia" "Pumpkin"]]
"-------- ----"
(re-split #" " fmt)
