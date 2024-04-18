(defn square [x] (* x x))
(defn cube [x] (* x x x))

(defn good_enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn good_enough_cube? [guess x]
  (< (abs (- (cube guess) x)) 0.001))

(defn good_enough_eps? 
  ([prev_guess curr_guess eps] (< (abs (- curr_guess prev_guess)) eps))
  ([prev_guess curr_guess] (< (abs (- curr_guess prev_guess)) 0.001))
)  ; Acts as a default argument for eps.

(defn improve [guess x]
  (/ (+ guess (/ x guess)) 2))

(defn improve_cube [guess x]
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(defn sqrt_iter [guess x]
  (if (good_enough? guess x) guess (sqrt_iter (improve guess x) x)))

(defn cbrt_iter [guess x]
  (if (good_enough_cube? guess x) guess (cbrt_iter (improve_cube guess x) x)))

(defn sqrt_iter_eps [guess_a guess_b x]
  (if (good_enough_eps? guess_a guess_b)
    guess_b
    (let [guess_a guess_b guess_b (improve guess_b x)]
    (sqrt_iter_eps guess_a guess_b x))
   )
)

(prn (sqrt_iter 1.0 2))
(prn (cbrt_iter 1.0 2))
(prn (sqrt_iter_eps 1.0 1.1 2))
