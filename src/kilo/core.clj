(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [flatland.useful.utils :as useful.utils]))


;;; utils

(defn exec [& args]
  (print "exec" args "\r\n")
  (flush)
  (let [pb (java.lang.ProcessBuilder. (into-array String args))]
    (.redirectInput pb java.lang.ProcessBuilder$Redirect/INHERIT)

    (let [p (.start pb)
          out (.getInputStream p)]
      (.waitFor p)
      (string/trim (slurp out)))))

(defn read1 [^java.io.BufferedReader reader]
  (loop [cnt 0]
    (when (and (not (.ready reader)) (< cnt 10))
      (print "waiting: " cnt "\r\n")
      (flush)
      (Thread/sleep 200)
      (recur (inc cnt))))
  (if (.ready reader)
    (.read reader)
    0))


;;; terminal

(defn enable-raw-mode []
  (useful.utils/returning
   (exec "/usr/bin/env" "stty" "-g")
   (exec "/usr/bin/env" "stty" "-brkint" "-icrnl" "-inpck" "-istrip" "-ixon")
   (exec "/usr/bin/env" "stty" "-opost")
   (exec "/usr/bin/env" "stty" "cs8")
   (exec "/usr/bin/env" "stty" "-echo" "-icanon" "-iexten" "-isig")
   (exec "/usr/bin/env" "stty" "min" "0")
   (exec "/usr/bin/env" "stty" "time" "1")))

(defn disable-raw-mode [terminal-config]
  (exec "/usr/bin/env" "stty" terminal-config))


;;; init

(defn -main [& args]
  (let [terminal-config (enable-raw-mode)]
    (print "terminal-config: " terminal-config "\r\n")
    (flush)
    (loop [inpt 0]
      (print "inpt: " inpt "\r\n")
      (flush)
      (when-not (= inpt (byte \q))
        (recur (read1 (java.io.BufferedReader. *in*)))))
    (disable-raw-mode terminal-config)))
