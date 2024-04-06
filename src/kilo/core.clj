(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [clojure.core.match :refer [match]]
   [flatland.useful.utils :as useful.utils]))


;;; data

(def kilo-version "0.0.1")

(def arrow-left (byte \a))
(def arrow-right (byte \d))
(def arrow-up (byte \w))
(def arrow-down (byte \s))

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
  (let [stdin (java.io.BufferedReader. *in*)
        c (read1 stdin)]
    (cond
      (= c 0x1b)
      (let [c0 (atom 0)
            c1 (atom 0)
            c2 (atom 0)]
        (or
         (when (and
                (not (= 0 (reset! c0 (read1 stdin))))
                (not (= 0 (reset! c1 (read1 stdin)))))
           (cond
             (and (= @c0 (byte \[)) (= @c1 (byte \A))) arrow-up
             (and (= @c0 (byte \[)) (= @c1 (byte \B))) arrow-down
             (and (= @c0 (byte \[)) (= @c1 (byte \C))) arrow-right
             (and (= @c0 (byte \[)) (= @c1 (byte \D))) arrow-left))
         c))
      :else c)))

(defn get-window-size []
  (let [res (exec "/usr/bin/env" "stty" "size")]
    (map #(Integer/parseInt %) (string/split res #"\s"))))


;;; output

(defn editor-draw-rows [buf]
  (dotimes [i (deref screen-rows)]
    (if (= i (int (/ (deref screen-rows) 3)))
      (let [welcome (str "Kilo editor -- version " kilo-version)
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
    (= c arrow-left) (swap! cx dec)
    (= c arrow-right) (swap! cx inc)
    (= c arrow-up) (swap! cy dec)
    (= c arrow-down) (swap! cy inc))
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
