(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [clojure.java.shell :as shell]))

(defn stty-exec [& args]
  (println (string/join " " args))
  (apply shell/sh "/usr/bin/env" "stty" args))

(defn byte-seq [^java.io.BufferedReader reader]
  (lazy-seq
   (if (not (.ready reader))
     (do
       (Thread/sleep 100)
       (cons 0 (byte-seq reader)))
     (let [ch (.read reader)]
       (if (= ch -1)
         '()
         (cons ch (byte-seq reader)))))))

(defn read1 [^java.io.BufferedReader reader]
  (loop [cnt 0]
    (when (and (not (.ready reader)) (< cnt 10))
      (println "waiting" cnt)
      (Thread/sleep 200)
      (recur (inc cnt))))
  (if (.ready reader)
    (.read reader)
    0))

(defn -main [& args]
  ;; (try
  ;;   (let [res (exec! stty-get-current-settings)]
  ;;     (try
  ;;       (println "hello, world")
  ;;       (exec! (fn [] stty-change-key-echo false))
  ;;       (doseq [elm (byte-seq (new java.io.BufferedReader *in*))]
  ;;         (when (= elm (int \q)) (System/exit 1))
  ;;         (println elm))
  ;;       (finally
  ;;         (exec! (fn [] (stty-set-current-settings res))))))
  ;;   (catch Exception e
  ;;     (println (.getMessage e))
  ;;     (System/exit 1)))
  (let [inpt (read1 (java.io.BufferedReader. *in*))]
    (println inpt)))
