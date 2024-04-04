(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]))


;;; utils

(defn exec [& args]
  (let [pb (java.lang.ProcessBuilder. (into-array String args))]
    (.redirectInput pb java.lang.ProcessBuilder$Redirect/INHERIT)

    (let [p (.start pb)
          out (.getInputStream p)]
      (.waitFor p)
      (string/trim (slurp out)))))

(defn read1 [^java.io.BufferedReader reader]
  (loop [cnt 0]
    (when (and (not (.ready reader)) (< cnt 10))
      (println "waiting" cnt)
      (Thread/sleep 200)
      (recur (inc cnt))))
  (if (.ready reader)
    (.read reader)
    0))


;;; init

(defn -main [& args]
  (let [res (exec "stty" "-g")]
    (println res))
  (let [inpt (read1 (java.io.BufferedReader. *in*))]
    (println inpt)))
