(ns clojure.sqlite
  (:import [java.sql DriverManager SQLException]
	   [org.sqlite JDBC])
  (:use [clojure.contrib.sql]))

(defn direct-port []
  (Class/forName "org.sqlite.JDBC")
  (let [conn (DriverManager/getConnection 
	      "jdbc:sqlite:/Users/shughes/Documents/clojure-trials/src/clojure/test.db")
	stat (doto (.createStatement conn)
	       (.executeUpdate "drop table if exists people;")
	       (.executeUpdate "create table people (name, occupation);"))
	prep (doto (.prepareStatement conn "insert into people values (?, ?);")
	       (.setString 1 "Gandhi")
	       (.addBatch))]
    (.setAutoCommit conn false)
    (.executeBatch prep)
    (.setAutoCommit conn true)
    (let [results (.executeQuery stat "select * from people")]
      (loop []
	(when (.next results)
	  (println "name =" (.getString results "name"))
	  (println "job  ="  (.getString results "occupation"))
	  (recur))))
    (.close conn)))

(defn contrib-sql []
  (with-connection {:classname   "org.sqlite.JDBC"
		    :subprotocol "sqlite"
		    :subname     "test.db"}
    (try
     (drop-table :people)
     (catch SQLException e))
    (create-table :people [:name :text] [:occupation :text])
    (insert-values :people
		   [:name          :occupation]
		   ["Gandhi"       "politics"]
		   ["Turing"       "computers"]
		   ["Wittgenstein" "smartypants"])
    (with-query-results results ["select * from people"]
      (doseq [record results]
	(println "name =" (:name record))
	(println "job  =" (:occupation record))))))

