;; Define some functions.
(defn arity_myplus 
  ([] "Supply some arguments!")
  ([a b] (+ a b))
  ([a b c] (+ a b c))
  )

(defn square [x] (* x x))
(defn sum_of_squares 
  ([& numbers] (apply + (map square numbers))))

(defn myabs [x] 
  (cond (> x 0) x
        :else (- x)
        )
)

(defn absif [x]
  (if (> x 0) x (- x)))

;; I wanted to see if you could simulate a ternary expression.
; (defn sick_absif [x]
;   (((pos? x) x) (- x)))

(defn allpositive? [& numbers]
  (every? pos? numbers))

(defn top_k_sum_of_squares [k & numbers]
  (apply sum_of_squares (take k (sort numbers))))

(defn good_enough? [guess x]
  (prn "Is guess " guess "good enough?")
  (< (abs (- (square guess) x)) 0.001))

(defn improve [guess x]
  (/ (+ x (/ x guess)) 2))

(defn sqrt_iter [guess x]
  (if (good_enough? guess x) guess (sqrt_iter (improve guess x) x)))

;; Print some outputs.
(prn (arity_myplus 1 2))
(prn (arity_myplus 1 2 3))
(prn (arity_myplus ))

(prn (sum_of_squares 3 4))
(prn (sum_of_squares 1 2 3 4))
(prn (myabs 3) (myabs -17))
(prn (absif 3) (absif -17))
; (prn (sick_absif 3) (sick_absif -17))
(prn (allpositive? 3) (allpositive? 3 -17 21))
(prn (top_k_sum_of_squares 3 1 7 3 8 2 9))
(prn (sqrt_iter 1 2))
