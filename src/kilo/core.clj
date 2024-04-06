(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [flatland.useful.utils :as useful.utils]))


;;; data

(def kilo_version "0.0.1")

(def cx (atom 0))
(def cy (atom 0))
(def screen-rows (atom 0))
(def screen-columns (atom 0))

(def debug-str (atom ""))


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
      (Thread/sleep 20)
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

(defn get-window-size []
  (let [res (exec "/usr/bin/env" "stty" "size")]
    (map #(Integer/parseInt %) (string/split res #"\s"))))


;;; output

(defn editor-draw-rows [buf]
  (dotimes [i (deref screen-rows)]
    (if (= i (int (/ (deref screen-rows) 3)))
      (let [welcome (str "Kilo editor -- version " kilo_version)
            padding (int (/ (- (deref screen-columns) (count welcome)) 2))]
        (.write buf "~")
        (.write buf (apply str (repeat (dec padding) " ")))
        (.write buf welcome))
      (.write buf "~"))
    (.write buf "\u001b[K")
    (when (< i (- (deref screen-rows) 1))
      (.write buf "\r\n"))
    (when (= i (dec (deref screen-rows)))
      (.write buf (format "x: %d, y: %d" (deref cx) (deref cy)))
      (when (not= (count @debug-str) 0)
        (.write buf (str " " @debug-str))))))

(defn editor-refresh-screen []
  (let [buf (java.io.BufferedWriter. *out*)]
    (.write buf "\u001b[?25l")
    (.write buf "\u001b[H")

    (editor-draw-rows buf)

    (.write buf (format "\u001b[%d;%dH" @cy @cx))
    (.write buf "\u001b[?25h")
    (.flush buf)))


;;; input

(defn editor-move-cursor [c]
  (cond
    (= c (byte \a)) (swap! cx dec)
    (= c (byte \d)) (swap! cx inc)
    (= c (byte \w)) (swap! cy dec)
    (= c (byte \s)) (swap! cy inc))
  c)

(defn editor-process-keypress []
  (let [c (editor-read-key)]
    (cond
      (= c (ctrl-key \q)) -1
      (= c (byte \a)) (editor-move-cursor c)
      (= c (byte \d)) (editor-move-cursor c)
      (= c (byte \w)) (editor-move-cursor c)
      (= c (byte \s)) (editor-move-cursor c)
      :else c)))


;;; init

(defn init-editor []
  (let [[rows columns] (get-window-size)]
    (reset! cx 0)
    (reset! cy 0)
    (reset! screen-rows rows)
    (reset! screen-columns columns)))

(defn -main [& args]
  (let [terminal-config (enable-raw-mode)]
    (init-editor)
    (loop [inpt 0]
      (when-not (= inpt -1)
        (editor-refresh-screen)
        (recur (editor-process-keypress))))
    (print "\u001b[2J")
    (print "\u001b[H")
    (flush)
    (disable-raw-mode terminal-config)))
