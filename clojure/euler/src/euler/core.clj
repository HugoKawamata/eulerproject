(ns euler.core
  (:gen-class))



(defn mult-3-5
  ;; PROBLEM 1
  ;; Mult 3 5
  [max]
  (loop [i 0
         sum 0]
    (if (>= i max)
      sum
      (if (or (= (mod i 3) 0) (= (mod i 5) 0))
        (recur (inc i) (+ sum i))
        (recur (inc i) sum)))))

(defn sum-even-fib
  ;; PROBLEM 2
  ;; Get sum of even fibonacci numbers up to max
  [max]
  (loop [i 1
         j 1
         sum 0]
    (if (> j max)
      sum
      (if (even? j)
        (recur j (+ j i) (+ sum j))
        (recur j (+ j i) sum)))))

(defn prime?
  [n]
  (if (= n 2)
    true
    (if (even? n)
      false
      (let [root (int (Math/sqrt n))]
        (loop [i 3]
          (if (> i root)
            true
            (if (zero? (mod n i))
              false
              (recur (+ i 2)))))))))

(defn next-prime
  [p]
  (if (prime? p)
    (if (or (= p 1) (= p 0))
      2
      (if (= p 2)
        3
        (loop [p2 (+ 2 p)]
          (if (prime? p2)
            p2
            (recur (+ 2 p2))))))
  nil))

(defn largest-prime-factor
  ;; PROBLEM 3
  ;; Find the Largest Prime Factor
  [n]
  (loop [big n
         lpf 2
         current 2]
    (if (> current big)
      lpf
      (if (zero? (mod big current))
        (recur (/ big current) current current)
        (recur big lpf (next-prime current))))))

(defn palindrome?
  [n]
  (loop [s (str n)]
    (if (= 1 (count s))
      true
      (if (= (subs s 0 1) (subs s (- (count s) 1) (count s)))
        (if (= 2 (count s))
          true
          (recur (subs s 1 (- (count s) 1))))
        false))))

(defn largest-palindrome-3digit-product
  ;; PROBLEM 4
  ;; Get largest palindrome that is the product of two 3-digit numbers
  []
  (loop [a 999
         b 999
         biggest 0]
    (if (< a 100)
      [biggest a b]
      (let [c (if (and (palindrome? (* a b)) (> (* a b) biggest))
            (* a b)
            biggest)]
        (if (= b 100)
          (recur (dec a) (dec a) c)
          (recur a (dec b) c))))))

(defn smallest-div-by-all
  ;; PROBLEM 5
  ;; Get smallest number divisible by 1 to n
  [n]
  (loop [i n]
    (if (loop [div n]
      (if (< div 11)
        true
        (if (zero? (mod i div))
          (recur (dec div))
          false)))
      i
      (recur (inc i)))))

(defn sum-of-squares
  [vec]
  (reduce + (map * vec vec)))

(defn square-of-sum
  [vec]
  (let [n (reduce + vec)]
    (* n n)))

(defn get-first-naturals
  ;; Gets the first n natural numbers
  [n]
  (loop [vec []
         i 1]
    (if (> i n)
      vec
      (recur (conj vec i) (inc i)))))

(defn diff-sums-squares
  ;; PROBLEM 6
  ;; Find difference between sum of squares of first n natural numbers and 
  ;; the square of the sum
  [n]
  (- (square-of-sum (get-first-naturals n)) (sum-of-squares (get-first-naturals n))))

(defn nth-prime
  ;; PROBLEM 7
  ;; Find nth prime number
  [n]
  (loop [p 2
         i 1]
    (if (= i n)
      p
      (recur (next-prime p) (inc i)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
