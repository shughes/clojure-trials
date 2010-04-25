(ns clojure.test)

(defstruct cd :title :artist :rating :ripped)

(def add-records (fn [db & cd] (into db cd)))

(defn init-db []
  (add-records #{}
	       (struct cd "Roses" "Kathy Mattea" 7 true)
	       (struct cd "Fly" "Dixie Chicks" 8 true)
	       (struct cd "Home" "Dixie Chicks" 9 true)))

(defn dump-db [db]
  (doseq [cd db]
    (doseq [[key value] cd]
      (print (format "%10s: %s\n" (name key) value)))
    (println)))

(defn derivative [f dx]
  (fn [x] (/ (- (f (+ x dx)) (f x)) dx)))

(def sam (derivative (fn [x] (* x x)) 10))

(def book "Harry Potter")

(defn filter1 [f lst]
  (cond (= lst ()) ()
	(f (first lst)) (concat (list (first lst)) (filter1 f (rest lst)))
	:else (filter1 f (rest lst))))

(def book-sales (fn [book]
		  (cond (= book "Harry Potter") 10
			:else 0)))

(def book-list (list "Sum of All Fears" "Harry Potter" "The Outliers"))

(defn best-selling-books [threshold book-list]
  (filter1 (fn [book]
	     (> (book-sales book) threshold))
	   book-list))

(defn filter2 [f lst]
  (loop [lst2 lst
	 new-lst ()]
    (let [fst (first lst2)
	  rst (rest lst2)]
      (cond (= fst nil) new-lst
	    (f fst) (recur rst (concat new-lst (list fst)))
	    :else (recur rst new-lst)))))

(filter2 (fn [x] (or (= x 2) (= x 3))) (list 3 5 2 1 4 2))

; list: (1 2 3 4 5)
; vector: [1 2 3 4 5]
; maps: {:a 1, :b 2, :c 3}
; sets: #{fred ethel lucy}

(def lst (list 1 2 3 4))

(def map1 {:a 1, :b 2, :c 3})
(def map2 {:d 4, :e 5, :f 6})

(.. System getProperties (get "java.version"))

(macroexpand '(when (pos? a) (println "positive") (/ b a)))

(import '(java.util HashMap))
(def mymap (new HashMap))
(. mymap (put "key" "value"))
(. mymap (get "key"))

;(doto mymap
;  (. put "1" "one")
;  (. put "2" "two")
;  (. put "3" "three"))
;(. mymap (get "3"))

(def map3 (-> {} (assoc :a 1) (assoc :b 2)))
;(def map3 (-> mymap (assoc :c 3)))

(defmacro our-when [test & body]
  `(if ~test (do ~@body)))

(macroexpand '(our-when (= 2 2) (+ 2 5) (+ 2 1)))
(our-when (= 2 2) (+ 2 5) (+ 2 1))

(defmacro my-dbg [& body]
  `(do ~@body))
(macroexpand '(my-dbg (+ 3 5) (+ 2 7)))

(defmacro my-defn [name args & body]
  `(def ~name (fn [~@args] ~@body)))

;; macros:
;; ~ says argument is function or variable?
;; @ lists values 
;; ` executes function?


(defmacro defn2 [name arg & body]
  `(def ~name (fn [~arg]
		~@body)))

(defn2 sam a
  (* a 25))

(sam 10)

(def lst '(a b c d e))
`[~@lst]

(def l 'a)
`~l

(def curry (fn [x]
	     (fn [y]
	       (+ x y))))
((curry 10) 20)

(def mytest (fn [fun]
	      (fun 10)))

(mytest (fn [x] (+ x 25)))

(defn name-summary [[name1 name2 & others]]
  (println (str name1 ", " name2) "and" (first others)))

;(name-summary ["Moe" "Larry" "Curly" "Shemp"])

;; macros

(defn divides? [candidate-divisor dividend]
  (zero? (rem dividend candidate-divisor)))

(defn prime? [num]
  (when (> num 1)
    (every? (fn [x] (not (divides? x num)))
	    (range 2 (inc (int (Math/sqrt num)))))))

(defn primes-from [number]
  (filter prime? (iterate inc number)))

;; tail recursion
(defn primes-in-range [start end]
  (loop [val start
	 lst '()]
    (cond (> val end) lst
	  (prime? val) (recur (+ 1 val) (concat lst (list val)))
	  :else (recur (+ 1 val) lst))))

;; non-tail recursion
(defn primes-in-range [start end]
  (cond (> start end) nil
	(prime? start) (concat (list start) (primes-in-range (+ 1 start) end))
	:else (primes-in-range (+ 1 start) end)))
     
(primes-in-range 5 97)

(defmacro do-primes [var start end & body]
  `(doseq [~var (primes-in-range ~start ~end)] ~@body))

(defmacro my-defn [func args & body]
  `(def ~func (fn [~@args] ~@body)))

(my-defn my-test [x y] 
  (if (= x y) "equals"
      "don't equal"))

(defmulti foo class)

(defmethod foo String [s] :a-string)

(foo "samuel")

(defn sam [x] 
  (if (= "sam" x) 
    "first name" 
    "last name"))

(defmulti tester sam)

(defmethod tester "first name" [x] 
  "good job")

(defmethod tester "last name" [x] 
  "bad job")

(tester "sam")

(defn my-multi [x]
  (x :value)) 

(defmulti foo my-multi)

(defmethod foo "samuel" [s] 
  "first name")

(defmethod foo "hughes" [s]
  "last name")

(def test1 {:value "hughes"})

(foo test1)