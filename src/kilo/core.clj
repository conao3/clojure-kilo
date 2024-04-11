(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [clojure.core.match :refer [match]]
   [flatland.useful.utils :as useful.utils]))


;;; data

(def KILO-VERSION "0.0.1")

(def ARROW-LEFT 1000)
(def ARROW-RIGHT 1001)
(def ARROW-UP 1002)
(def ARROW-DOWN 1003)
(def DEL-KEY 1004)
(def HOME-KEY 1005)
(def END-KEY 1006)
(def PAGE-UP 1007)
(def PAGE-DOWN 1008)

(def cx (atom 0))
(def cy (atom 0))
(def numrows (atom 0))
(def row (atom ""))
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
             (and (= @c0 (byte \[)) (<= (byte \0) @c1) (<= @c1 (byte \9)))
             (when (not (= 0 (reset! c2 (read1 stdin))))
               (reset! debug-str (str "ESC [ " @c1 " " @c2))
               (cond
                 (and (= @c1 (byte \1)) (= @c2 (byte \~))) HOME-KEY
                 (and (= @c1 (byte \3)) (= @c2 (byte \~))) DEL-KEY
                 (and (= @c1 (byte \4)) (= @c2 (byte \~))) END-KEY
                 (and (= @c1 (byte \5)) (= @c2 (byte \~))) PAGE-UP
                 (and (= @c1 (byte \6)) (= @c2 (byte \~))) PAGE-DOWN
                 (and (= @c1 (byte \7)) (= @c2 (byte \~))) HOME-KEY
                 (and (= @c1 (byte \8)) (= @c2 (byte \~))) END-KEY))
             (and (= @c0 (byte \[)) (= @c1 (byte \A))) ARROW-UP
             (and (= @c0 (byte \[)) (= @c1 (byte \B))) ARROW-DOWN
             (and (= @c0 (byte \[)) (= @c1 (byte \C))) ARROW-RIGHT
             (and (= @c0 (byte \[)) (= @c1 (byte \D))) ARROW-LEFT
             (and (= @c0 (byte \[)) (= @c1 (byte \H))) HOME-KEY
             (and (= @c0 (byte \[)) (= @c1 (byte \F))) END-KEY

             (and (= @c0 (byte \O)) (= @c1 (byte \H))) HOME-KEY
             (and (= @c0 (byte \O)) (= @c1 (byte \F))) END-KEY))
         c))
      :else c)))

(defn get-window-size []
  (let [res (exec "/usr/bin/env" "stty" "size")]
    (map #(Integer/parseInt %) (string/split res #"\s"))))


;;; file i/o

(defn editor-open []
  (reset! row "Hello, world!")
  (reset! numrows 1))


;;; output

(defn editor-draw-rows [buf]
  (dotimes [i (deref screen-rows)]
    (if (>= i @numrows)
      (if (= i (int (/ (deref screen-rows) 3)))
        (let [welcome (str "Kilo editor -- version " KILO-VERSION)
              padding (int (/ (- (deref screen-columns) (count welcome)) 2))]
          (.write buf "~")
          (.write buf (apply str (repeat (dec padding) " ")))
          (.write buf welcome))
        (.write buf "~"))
      (.write buf (deref row)))
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
    (= c ARROW-LEFT) (swap! cx #(max 0 (dec %)))
    (= c ARROW-RIGHT) (swap! cx #(min @screen-columns (inc %)))
    (= c ARROW-UP) (swap! cy #(max 0 (dec %)))
    (= c ARROW-DOWN) (swap! cy #(min @screen-rows (inc %))))
  c)

(defn editor-process-keypress []
  (let [c (editor-read-key)]
    (cond
      (= c (ctrl-key \q)) -1
      (or
       (= c ARROW-LEFT)
       (= c ARROW-RIGHT)
       (= c ARROW-UP)
       (= c ARROW-DOWN))
      (editor-move-cursor c)
      (= c HOME-KEY) (reset! cx 0)
      (= c END-KEY) (reset! cx @screen-columns)
      (= c PAGE-UP) (dotimes [_ @screen-rows] (editor-move-cursor ARROW-UP))
      (= c PAGE-DOWN) (dotimes [_ @screen-rows] (editor-move-cursor ARROW-DOWN))
      :else c)))


;;; init

(defn init-editor []
  (let [[rows columns] (get-window-size)]
    (reset! cx 0)
    (reset! cy 0)
    (reset! numrows 0)
    (reset! row (atom ""))
    (reset! screen-rows rows)
    (reset! screen-columns columns)))

(defn -main [& args]
  (let [terminal-config (enable-raw-mode)]
    (init-editor)
    (editor-open)
    (loop [inpt 0]
      (when-not (= inpt -1)
        (editor-refresh-screen)
        (recur (editor-process-keypress))))
    (print "\u001b[2J")
    (print "\u001b[H")
    (flush)
    (disable-raw-mode terminal-config)))
