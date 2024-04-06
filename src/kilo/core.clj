(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [flatland.useful.utils :as useful.utils]))


;;; utils

(defn ctrl-key [ch]
  (bit-and (byte ch) 0x1f))

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

(defn editor-read-key []
  (read1 (java.io.BufferedReader. *in*)))

(defn editor-process-key []
  (let [c (editor-read-key)]
    (cond
      (= c (ctrl-key \q)) -1
      :else c)))


;;; output

(defn editor-draw-rows []
  (dotimes [i 24]
    (print "~\r\n")))

(defn editor-refresh-screen []
  (print "\u001b[2J")
  (print "\u001b[H")

  (editor-draw-rows)

  (print "\u001b[H")
  (flush))


;;; init

(defn -main [& args]
  (let [terminal-config (enable-raw-mode)]
    (loop [inpt 0]
      (when-not (= inpt -1)
        (editor-refresh-screen)
        (recur (editor-process-key))))
    (print "\u001b[2J")
    (print "\u001b[H")
    (flush)
    (disable-raw-mode terminal-config)))
