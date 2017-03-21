(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [a n k]
                 (if (zero? k)
                   a
                   (recur (* a n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [k s]
                 (if (empty? s)
                   k
                   (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a b]
                 (cond 
                   (and (empty? a) (empty? b)) true
                   (or (empty? a) (empty? b)) false
                   (not (= (first a) (first b))) false
                   :else (recur (rest a) (rest b))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [n 0]
    (if (>= n (count a-seq))
      nil
      (if (pred (get a-seq n))
        n
        (recur (inc n))))))

(defn avg [a-seq]
  (loop [n (count a-seq)
         sum 0
         i 0]
    (if (>= i n)
      (/ sum n)
      (recur n (+ sum (get a-seq i)) (inc i)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         i 0]
    (if (>= i (count a-seq))
      a-set
      (recur (toggle a-set (get a-seq i)) (inc i)))))

(defn fast-fibo [n]
  (loop [fa 0
         fb 1
         i 2]
    (cond
      (= n 0) 0
      (>= i n) (+ fa fb)
      :else (recur fb (+ fa fb) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [a-set []
         i 0]
    (if (>= i (count a-seq))
      a-set
      (let [a (get a-seq i)]
        (if (some (fn [x] (= a x)) a-set)
          a-set
          (recur (conj a-set a) (inc i)))))))

