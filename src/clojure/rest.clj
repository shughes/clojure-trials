(ns clojure.rest
  (:use [clojure.http.client]
	[clojure.contrib.json.write]
	[clojure.contrib.str-utils]
	[clojure.contrib.json.read])
  (:require [clojure.http.resourcefully :as network]
	    [clojure.contrib.zip-filter.xml :as contrib-xml]))

;; (network/get "http://localhost:8080/accounts/")

;; (network/post "http://localhost:8080/accounts/shughes/"
;;	      {"Content-type" "text/json"} (json-str {:owner "Samuel Jennings Hughes"}))

; (def result (read-json (first ((network/get "http://twitter.com/users/show.json?screen_name=samofficial") :body-seq))))

;; echo -n "username:password" | openssl enc -base64

(def auth "Basic c2Ftb2ZmaWNpYWw6c3RvazYzOTQ=")

(defstruct twitter :post_id :screen_name)

(defn post-update [status]
  (let [response (network/post "http://twitter.com/statuses/update.json" 
			       {"Authorization" auth}
			       {"status" status})
	result (read-json (first (response :body-seq)))]
    (struct twitter (result "id") ((result "user") "screen_name"))))

; (def response (post-update "testing again"))

; (network/delete (str "http://twitter.com/statuses/destroy/" (response :post_id) ".json")
;		{"Authorization" auth})

;; xml

(def file (slurp "/Users/shughes/Desktop/local.xml"))
(re-seq #"\<|\w+|\>" file)

(defn parse [file]
  (loop [file-str (slurp file)
	 word ""
	 result '()]
    (cond (= file-str ()) result
	  (= (str (first file-str)) " ") (if (not= word "") 
					   (recur (rest file-str) "" (concat result (list word)))
					   (recur (rest file-str) "" result))
	  :else (if (and (not= "\n" (str (first file-str))) 
			 (not= "\t" (str (first file-str))))
		  (recur (rest file-str) (str word (first file-str)) result)
		  (recur (rest file-str) word result)))))

(defstruct word-group :word :rest)

(defn next-word [xml-str]
  (loop [file-str xml-str
	 word ""]
    (let [trim-w (. word trim)]
      (if (= file-str "") (struct word-group trim-w file-str)
	  (let [first-char (subs file-str 0 1)
		rest-str (subs file-str 1)]
	    (cond (or (= first-char "<") (= first-char ">") (= first-char "/"))
		  (if (= trim-w "") (recur rest-str word) (struct word-group trim-w rest-str))
		  :else (recur rest-str (str word first-char))))))))

(defn parse [file]
  (loop [file-str (slurp file)
	 result '()]
    (let [word-grp (next-word file-str)
	  word (word-grp :word)
	  new-file-str (word-grp :rest)]
      (if (= "" new-file-str) result
	  (recur new-file-str (concat result (list word)))))))

(defn sfirst [str]
  (if (not= "" str) 
    (subs str 0 1)
    nil))

(defn srest [str-val]
  (if (not= "" str-val)
    (subs str-val 1)
    nil))

(defstruct element-node :tag :attrs :content)
(defstruct result-node :element :xml)

(defn get-element [str-val]
  (loop [xml-str str-val, element ""]
    (cond (= xml-str "") (struct result-node element xml-str)
	  (= (sfirst xml-str) ">") (struct result-node (str element ">") (srest xml-str))
	  :else (recur (srest xml-str) (str element (sfirst xml-str))))))

(defn get-text [str-val]
  (loop [xml-str str-val, text ""]
    (cond (= xml-str "") (struct result-node (. text trim) xml-str)
	  (= (sfirst xml-str) "<") (struct result-node (. text trim) xml-str)
	  :else (recur (srest xml-str) (str text (sfirst xml-str))))))

(defn startparse [file]
  (loop [xml-str (slurp file), result '()]
    (cond (= xml-str "") result
	  (= "<" (sfirst xml-str)) (let [element-set (get-element xml-str)]
				     (recur (element-set :xml) (concat result (list (element-set :element)))))
	  :else (let [text-node (get-text xml-str)] 
		  (if (= "" (text-node :element))
		    (recur (text-node :xml) result)
		    (recur (text-node :xml) (concat result (list (text-node :element)))))))))

(def parse-list (startparse "/Users/shughes/Desktop/local2.xml"))

(defn begin-element? [txt]
  (let [a (str (first txt)), b (str (first (rest txt)))]
    (and (= a "<") (not= b "/"))))

(defn end-element? [txt]
  (let [a (str (first txt)), b (str (first (rest txt)))]
    (and (= a "<") (= b "/"))))

(defn text? [txt]
  (not= (str (first txt)) "<"))

(defn element-val [txt]
  (cond (= txt "") ""
	(= (subs txt 0 1) "<") (element-val (subs txt 1))
	(= (subs txt 0 1) ">") (element-val (subs txt 1))
	(= (subs txt 0 1) "/") (element-val (subs txt 1))
	:else (str (subs txt 0 1) (element-val (subs txt 1)))))

(defn parse [xml-lst]
  (let [x-first (first xml-lst)
	result (cond (= xml-lst ()) nil
		     (begin-element? x-first) (struct element-node 
						      (element-val x-first)
						      nil [(parse (rest xml-lst))])
		     (text? x-first) x-first
		     :else (parse (rest xml-lst)))]
    (println result)
    (if (not= nil result)
      (parse (rest xml-lst)))
    result))

(parse parse-list)


(clojure.xml/parse "file:///Users/shughes/Desktop/local.xml")

(def xml (clojure.zip/xml-zip (clojure.xml/parse "/Users/shughes/Desktop/local.xml")))
(xml 0)
(contrib-xml/xml-> xml :channel :item)

;; imap
(add-classpath "file:///usr/local/javamail/mail.jar")
(add-classpath "file:///usr/local/share/java/spring-framework-2.5.1/lib/j2ee/activation.jar")
(import '(java.util Properties)
	'(javax.mail Folder Message MessagingException Session Store))

(def props (. System getProperties))
(. props (setProperty "mail.store.protocol" "imaps"))
(def session (. Session (getDefaultInstance props, nil)))
(def store (. session (getStore "imap")))
;(. store (connect "mail.samueljennings.com" "sam" "stok6394"))

;(def folder (. store getDefaultFolder))
;(def inbox (. folder (getFolder "INBOX")))
;(. inbox (open (. Folder READ_ONLY)))
;(def messages (. inbox getMessages))

(defn iterate-messages [messages]
  (cond (= (count messages) 0) "finished"
	:else (let [message (first messages)]
		(println (. message getSubject) (. message getMessageID))
		(iterate-messages (rest messages)))))
