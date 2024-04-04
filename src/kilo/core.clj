(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [flatland.useful.utils :as useful.utils]))


;;; utils

(defn exec [& args]
  (println "exec" args)
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


;;; terminal

(defn enable-raw-mode []
  (useful.utils/returning (exec "/usr/bin/env" "stty" "-g")
    (exec "/usr/bin/env" "stty" "-echo" "-icanon" "min" "1")))

(defn disable-raw-mode [terminal-config]
  (exec "/usr/bin/env" "stty" terminal-config))


;;; init

(defn -main [& args]
  (let [terminal-config (enable-raw-mode)]
    (println "terminal-config: " terminal-config)
    (let [inpt (read1 (java.io.BufferedReader. *in*))]
      (println inpt))
    (disable-raw-mode terminal-config)))
