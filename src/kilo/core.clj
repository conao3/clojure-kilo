(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]
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
(def rx (atom 0))
(def rowoff (atom 0))
(def coloff (atom 0))
(def screen-rows (atom 0))
(def screen-columns (atom 0))
(def numrows (atom 0))

(def tab-stop (atom 8))
(def debugp (atom true))

(defrecord Row [chars render])
(def row (atom []))                     ; list of Row
(def filename (atom nil))

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


;;; row operations

(defn editor-row-cx-to-rx [row cx]
  (let [rx (atom 0)]
    (doseq [s (subs (:chars row) 0 cx)]
      (swap! rx inc)
      (when (= s \tab)
        (swap! rx #(+ % (- @tab-stop (mod % @tab-stop))))))
    @rx))


;;; file i/o

(defn chars->Row [chars]
  (let [lst (atom '())
        lstsize (atom 0)]
    (doseq [c chars]
      (cond
        (= c \tab)
        (do
          (swap! lst conj " ")          ; min space size
          (dotimes [_ (- @tab-stop (mod (count @lst) @tab-stop))]
            (swap! lst conj " ")))
        true
        (do
          (swap! lst conj c)
          (swap! lstsize inc))))
    (let [render (string/join (reverse @lst))]
      (map->Row
       {:size (count chars)
        :rsize (count render)
        :chars chars
        :render render}))))

(defn editor-open [filename_]
  (reset! row (map chars->Row (string/split-lines (slurp filename_))))
  (reset! numrows (count @row))
  (reset! filename filename_))


;;; output

(defn editor-scroll []
  (reset! rx 0)
  (when (and (not (= 0 @numrows)) (< @cy @numrows))
    (reset! rx (editor-row-cx-to-rx (nth @row @cy) @cx)))
  (when (< @cy @rowoff)
    (reset! rowoff @cy))
  (when (<= (+ @rowoff @screen-rows) @cy)
    (reset! rowoff (inc (- @cy @screen-rows))))
  (when (< @rx @coloff)
    (reset! coloff @rx))
  (when (<= (+ @coloff @screen-columns) @rx)
    (reset! coloff (inc (- @rx @screen-columns)))))

(defn editor-draw-rows [buf]
  (dotimes [i @screen-rows]
    (let [filerow (+ i @rowoff)]
      (if (>= filerow @numrows)
        (if (and (= 0 @numrows) (= i (int (/ @screen-rows 3))))
          (let [welcome (str "Kilo editor -- version " KILO-VERSION)
                padding (int (/ (- @screen-columns (count welcome)) 2))]
            (.write buf "~")
            (.write buf (apply str (repeat (dec padding) " ")))
            (.write buf welcome))
          (.write buf "~"))
        (let [trow (nth @row filerow)
              len (:rsize trow)
              start (min len @coloff)
              end (min (+ @screen-columns @coloff) len)]
          (.write buf (subs (:render trow) start end)))))
    (.write buf "\u001b[K")
    (.write buf "\r\n"))
  (when @debugp
    (let [debug (format "cx: %d, cy: %d, rx: %d, numrows: %d, rowoff: %d, coloff: %d %S" @cx @cy @rx @numrows @rowoff @coloff @debug-str)]
      (.write buf (subs debug 0 (min @screen-columns (count debug))))
      (.write buf "\r\n"))))

(defn editor-draw-status-bar [buf]
  (.write buf "\u001b[7m")
  (let [s (format "%S - %d lines"
                  (or @filename "[No Name]")
                  @numrows)]
    (.write buf (subs s 0 (min @screen-columns (count s))))
    (.write buf (apply str (repeat (- @screen-columns (count s)) " "))))
  (.write buf "\u001b[m"))

(defn editor-refresh-screen []
  (editor-scroll)
  (let [buf (java.io.BufferedWriter. *out*)]
    (.write buf "\u001b[?25l")
    (.write buf "\u001b[H")

    (editor-draw-rows buf)
    (editor-draw-status-bar buf)

    (.write buf (format "\u001b[%d;%dH" (inc (- @cy @rowoff)) (inc (- @rx @coloff))))
    (.write buf "\u001b[?25h")
    (.flush buf)))


;;; input

(defn editor-move-cursor [c]
  (cond
    (= c ARROW-LEFT)
    (do
      (if (not (= 0 @cx))
        (swap! cx #(max 0 (dec %)))
        (when (< 0 @cy)
          (swap! cy dec)
          (when (< @cy @numrows)
            (reset! cx (:size (nth @row @cy)))))))
    (= c ARROW-RIGHT)
    (do
      (when (< @cy @numrows)
        (if (< @cx (:size (nth @row @cy)))
          (swap! cx inc)
          (when (< @cy @numrows)
            (swap! cy inc)
            (reset! cx 0)))))
    (= c ARROW-UP) (swap! cy #(max 0 (dec %)))
    (= c ARROW-DOWN) (swap! cy #(min (dec @numrows) (inc %))))
  (when (and (not (= 0 @numrows)) (< @cy @numrows))
    (let [trow (nth @row @cy)]
      (when (< (:size trow) @cx)
        (reset! cx (:size trow)))))
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
      (= c END-KEY) (reset! cx (:size (nth @row @cy)))
      (= c PAGE-UP)
      (do
        (reset! cy @rowoff)
        (dotimes [_ @screen-rows] (editor-move-cursor ARROW-UP)))
      (= c PAGE-DOWN)
      (do
        (reset! cy (min (dec @numrows) (dec (+ @rowoff @screen-rows))))
        (dotimes [_ @screen-rows] (editor-move-cursor ARROW-DOWN)))
      :else c)))


;;; init

(defn init-editor []
  (let [[rows columns] (get-window-size)]
    (reset! cx 0)
    (reset! cy 0)
    (reset! rx 0)
    (reset! rowoff 0)
    (reset! coloff 0)
    (reset! numrows 0)
    (reset! row [])
    (reset! filename nil)
    (reset! screen-rows (- rows 1 (if @debugp 1 0)))
    (reset! screen-columns columns)))

(defn -main [& args]
  (let [terminal-config (enable-raw-mode)]
    (init-editor)
    (if (<= 1 (count args))
      (editor-open (nth args 0)))
    (loop [inpt 0]
      (when-not (= inpt -1)
        (editor-refresh-screen)
        (recur (editor-process-keypress))))
    (print "\u001b[2J")
    (print "\u001b[H")
    (flush)
    (disable-raw-mode terminal-config)))
