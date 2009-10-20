(ns db.file
  (:use [clojure.contrib.str-utils]
	[clojure.contrib.math])
  (:import [java.nio.channels FileChannel]
	   [java.nio ByteBuffer]
	   [java.util BitSet]
	   [java.io File FileInputStream FileOutputStream DataOutputStream BufferedInputStream]))

(def *page-size* 1024)

(def *leaf-flag* (byte 8))

(defn- is-leaf? [b]
  (if (> (bit-and *leaf-flag* b) 0) true false))

(defn- sfirst [val]
  (if (= 0 (count val)) nil
      (subs val 0 1)))

(defn- srest [str-val]
  (if (not= "" str-val)
    (subs str-val 1)
    nil))

(defn- dec-to-hex [i]
  (let [val (if (< i 0)
	      (+ 256 i)
	      i)]
    (loop [dec val, result ""]
      (let [ret (int (/ dec 16))
	    r1 (mod dec 16)
	    rem (cond (= r1 10) "a"
		      (= r1 11) "b"
		      (= r1 12) "c"
		      (= r1 13) "d"
		      (= r1 14) "e"
		      (= r1 15) "f"
		      :else r1)]
	(if (= 0 ret)
	  (str rem result)
	  (recur ret (str rem result)))))))

(defn- byte-list 
  "Converts a base 10 number into a list of bytes."
  [n]
  (loop [val n, result '()]
    (if (= val 0) result
	(let [rem (mod val 256)
	      q (int (/ val 256))]
	  (recur q (cons (byte rem) result))))))

(defn- rev-str [val]
  (if (= nil (sfirst val))
    nil
    (concat (rev-str (srest val)) (list (sfirst val)))))

(defn- hex-to-dec [h]
  (let [rlst (rev-str h)]
    (loop [i 0
	   r rlst
	   result 0]
      (if (= (first r) nil)
	result
	(let [val (cond (= (first r) "a") 10
			(= (first r) "b") 11
			(= (first r) "c") 12
			(= (first r) "d") 13
			(= (first r) "e") 14
			(= (first r) "f") 15
			:else (first r))]
	  (recur (inc i) (rest r) 
		 (+ (* (new Integer val) (expt 16 i)) result)))))))

(defn- split-hex 
  "Takes hex string and splits it into pairs. So x85a3 becomes (\"85\", \"a3\")."
  [hex]
  (let [even (if (= 0 (mod (count hex) 2))
	       hex
	       (str "0" hex))]
    (loop [h even
	   val ""
	   result '()]
      (cond 
	(= nil h) result
	(= (count val) 2) (recur (srest h) (sfirst h) (concat result (list val)))
	:else (recur (srest h) (str val (sfirst h)) result)))))

(defn- set-page-size 
  "Byte at index 16 and 17 give the page size."
  [ch b]
  (if (or (< b 512) (> b 65535))
    (throw (new Exception "Page size must be between 512 and 65,536"))
    (let [barr (byte-list b)
	  bb (. ByteBuffer (allocate 2))]
      (. bb (put 0 (first barr)))
      (. bb (put 1 (first (rest barr))))
      (.rewind bb)
      (. ch (position 16))
      (loop []
	(if (.hasRemaining bb)
	  (do 
	    (. ch (write bb))
	    (recur)))))))

(defn- get-page-size [ch]
  (. ch (position 16))
  (let [bb (. ByteBuffer (allocate 2))]
    (. ch (read bb))
    (let [h1 (dec-to-hex (. bb (get 0)))
	  h2 (dec-to-hex (. bb (get 1)))
	  h3 (if (= (count h1) 1) (str "0" h1) h1)
	  h4 (if (= (count h2) 1) (str "0" h2) h2)
	  hex (str h3 h4)]
      (hex-to-dec hex))))

(defn- set-header-string [ch str]
  (. ch (position 0))
  (let [bb (. ByteBuffer (allocate 16))]
    (. bb (put (.getBytes str)))
    (.rewind bb)
    (loop []
      (if (.hasRemaining bb)
	(do
	  (. ch (write bb))
	  (recur))))))

(defn- get-header-string [ch]
  (. ch (position 0))
  (let [bb (. ByteBuffer (allocate 16))]
    (. ch (read bb))
    (.rewind bb)
    (let [result (loop [arr []]
		   (if (.hasRemaining bb)
		     (recur (conj arr (.get bb)))
		     arr))]
      (new String (into-array Byte/TYPE result)))))

(defn- write-bytes [ch-out bb]
  (loop []
    (if (.hasRemaining bb)
      (do
	(. ch-out (write bb))
	(recur)))))

(defn- fill-string-data [bb page]
  (loop [i 0, pos *page-size*, result []]
    (if (not= i (count (page :data)))
      (let [data ((page :data) i)
	    size (+ (count data) 1
		    (if (is-leaf? (page :flags)) 0 4))]
	(. bb (position (- pos size)))
	(if (not (is-leaf? (page :flags)))
	  (. bb (put (into-array Byte/TYPE (list (byte 0) (byte 0) (byte 0) (byte 0))))))
	(. bb (put (byte (count data))))
	(. bb (put (.getBytes data)))
	(recur (inc i) (- pos size) (conj result (- pos size))))
      result)))

(defn- n-byte-array [n num]
  (let [bytes (byte-list num)
	max (- n (count bytes))
	byte-vec (loop [i 0, arr bytes]
		   (if (= i max)
		     arr
		     (recur (inc i) (concat (list (byte 0)) arr))))]
    (into-array Byte/TYPE byte-vec)))

(defn set-page
  [page]
  (let [bb (. ByteBuffer (allocate *page-size*))]
    (. bb (put (page :flags)))
    (. bb (put (n-byte-array 4 (page :parent))))
    (let [ptrs (fill-string-data bb page)]
      (. bb (position 7))
      (dotimes [i (count ptrs)]
	(. bb (put (n-byte-array 2 (ptrs i)))))
      ;; next available byte goes into byte offset 5.
      (. bb (position 5))
      (. bb (put (n-byte-array 2 (+ 7 (* 2 (count ptrs)))))))
    ;; write to file
    (. bb (position 0))
    (. (page :out) (position (- (+ 100 (* (page :id) 
					  *page-size*))
				*page-size*)))
    (write-bytes (page :out) bb)))

(defn- bytes-to-int [lst]
  (loop [i 0, rev (reverse lst), result 0]
    (if (= nil (first rev))
      result
      (let [val (if (< (first rev) 0)
		  (+ 256 (first rev))
		  (first rev))]
	(recur (inc i) (rest rev) (+ result (* (expt 256 i) val)))))))

(defn- get-free-byte [bb]
  (. bb (position 5))
  (let [bytes (list (.get bb) (.get bb))]
    (bytes-to-int bytes)))

(defn- get-page-index [id]
  (- (+ 100 (* id *page-size*)) *page-size*))

(defn- get-flag [bb]
  (. bb (position 0))
  (.get bb))

(defn- get-content [bb flags]
  (. bb (position 7))
  (let [free (get-free-byte bb)
	p (loop [i 7, result '()]
	    (if (= i free)
	      result
	      (let [offset (bytes-to-int [(.get bb) (.get bb)])]
		(recur (+ 2 i) (concat result (list offset))))))]
    (loop [ptrs p, result '()]
      (if (= (first ptrs) nil)
	result
	(do 
	  (. bb (position (first ptrs)))
	  (let [size (if (is-leaf? flags) (.get bb) 
			 (do (. bb (position (+ 4 (.position bb))))
			     (.get bb)))
		lst (loop [i 0, c '()]
		      (if (= i (- size 1))
			(concat c (list (.get bb)))
			(recur (inc i) (concat c (list (.get bb))))))
		content (new String (into-array Byte/TYPE lst))]
	    (recur (rest ptrs) (concat result (list content)))))))))

(defn- get-parent-id [bb]
  (. bb (position 1))
  (bytes-to-int (list (.get bb) (.get bb) (.get bb) (.get bb))))

(defn get-page [in id]
  (. in (position (get-page-index id)))
  (let [bb (. ByteBuffer (allocateDirect *page-size*))]
    (. in (read bb))
    (let [content (get-content bb (get-flag bb))
	  parent (get-parent-id bb)]
      ;; here
      )))

(defn test- [f]
  (let [buf (. ByteBuffer (allocateDirect (.length f)))
	fin (new FileInputStream f)
	fc (.getChannel fin)]
    (. fc (read buf))
    (. buf (position 0))
    (.close fc)
    (loop [result '()]
      (if (.hasRemaining buf)
	(recur (concat result (list (.get buf))))
	result))))

(def file (new File "/Users/shughes/Desktop/test.db"))
(def fout (new FileOutputStream file))
(def channel (.getChannel fout))
(set-header-string channel "SQLite format 3")
(set-page-size channel 1024)

(def -page {:out channel
	    :flags (byte 8)
	    :id 1
	    :parent 0
	    :data ["animals" "facebook" "samuel"]
	    :children nil})

(set-page -page)
(get-page chin 1)

(def ret (vec (test- file)))

(def fin (new FileInputStream file))
(def chin (.getChannel fin))
(get-page-size chin)
(get-header-string chin)