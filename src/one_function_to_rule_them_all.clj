(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (if (empty? a-seq)
    ()
    (reduce concat () a-seq)))

(defn str-cat [a-seq]
  (let [cat-helper (fn [f r]
                     (str f " " r))]
    (if (empty? a-seq)
      ""
      (reduce cat-helper a-seq))))

(defn my-interpose [x a-seq]
  (let [helper (fn [f r]
                 (if (empty? f)
                   (conj f r)
                   (conj f x r)))]
    (if (empty? a-seq)
      []
      (reduce helper [] a-seq))))

(defn my-count [a-seq]
  (let [helper (fn [acc r]
                 (+ acc 1))]
    (if (empty? a-seq)
      0
      (reduce helper 0 a-seq))))

(defn my-reverse [a-seq]
  (let [helper (fn [result r]
                 (cons r result))]
    (reduce helper [] a-seq)))

(defn min-max-element [a-seq]
  (let [helper (fn [[mi ma] value]
                 [(min mi value) (max ma value)])]
    (reduce helper [(first a-seq) (first a-seq)] a-seq)))

;(defn insert [sorted-seq n]
;  (let [helper (fn [value]
;                 (< value n))
;        begin (take-while helper sorted-seq)]
;    (concat begin [n] (drop (count begin) sorted-seq))))

(defn insert [sorted-seq n]
  (let [splitted (split-with (partial > n) sorted-seq)]
    (apply concat (first splitted) [n] (rest splitted))))

(defn insertion-sort [a-seq]
  (reduce insert nil a-seq))

(defn parity [a-seq]
  (let [helper (fn [result value]
                 (if (result value)
                   (disj result value)
                   (conj result value)))]
    (reduce helper #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([& vars]
   (reduce (fn [arg-count arg]
             (+ arg-count 1))
           0 vars)))

(defn my-*
  [& vars]
  (reduce (fn [product value]
            (* product value))
          1 vars))

(defn pred-and
  ([& funcs]
   (fn [x] (reduce (fn [ands func]
                     (and ands (func x))) () funcs))))

;(defn pred-and
;  ([& funcs]
;   (fn [x] (
;            (let [helper (fn [ands func]
;                           (and ands (func x)))]
;              (reduce helper (true) funcs))))))

(defn my-map
  ([f & a-seq]
   (reduce (fn [result value]
             ;(conj result (apply f value))
             (apply f value))
           [] a-seq)))
