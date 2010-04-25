(ns test
  (:require [clojure.contrib.monads :as m]))

(defn qsort [lst] 
  (let [l (first lst), f (fn [f2] (filter (fn [x] (f2 x l)) (rest lst)))]
	(if (= nil l) [] (concat (qsort (f <=)) [l] (qsort (f >))))))

(def lst [3 8 1 6 9 4 7 8])

(qsort lst)

;; identity monad
(defn m-bind [value function]
  (function value))

(m-bind 1 (fn [a] (m-bind (inc a) (fn [b] (* a b)))))

(let [a 1
      b (inc a)]
  (* a b))

;; identity monad
(defn fi [x]
  (m/domonad m/identity-m 
	     [a x, b (inc a)]
	     (* a b)))

(fi nil)

;; maybe monad
;; if nil then returns nil
(defn fm [x] 
  (m/domonad m/maybe-m
	     [a x, b (inc a)]
	     (* a b)))

(fm nil)

(for [a (range 5)
      b (range a)]
  (* a b))

;; sequence monad
(macroexpand-1 '(m/domonad m/sequence-m
	   [a (range 5)
	    b (range a)]
	   (* a b)))

(defn m-bind [s f]
  (apply concat (map f s)))

(defn m-result [value]
  (list value))

(m-bind (range 5) (fn [a] (m-bind (range a) (fn [b] (list (* a b))))))

(apply (fn [x] (* x x)) 

(range 5)

(m-bind (list 0 1 2 3 4) (fn [a] (m-bind (range a) (fn [b] (list (* a b)))))

(m-bind '(0 1 2 3 4) (fn [b] (list (* 5 b))))

(apply concat (map (fn [b] (list (* 5 b))) '(0 1 2 3 4))

;; when not sure how many arguments function f will have, 
;; use use apply, with the function and a list of its args.
(defn f [a b c] (* a b c))


;; this...
(apply concat (map (fn [b] (list (* 5 b))) '(0 1 2 3 4)))
;; same as this...
(concat '(0) '(5) '(10) '(15) '(20))