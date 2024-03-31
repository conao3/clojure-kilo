(ns kilo.core
  (:gen-class)
  (:require
   [clojure.string :as string]
   [clojure.java.shell :as shell]))

(defn stty-exec [& args]
  (println (string/join " " args))
  (apply shell/sh "/usr/bin/env" "stty" args))

(defn stty-get-current-settings []
  (stty-exec "-g"))

(defn stty-set-current-settings [settings]
  (stty-exec settings))

(defn stty-change-key-echo [enabledp]
  (stty-exec (if enabledp "echo" "-echo")))

(defn stty-change-canonical-mode [enabledp]
  (stty-exec (if enabledp "icanon" "-icanon"))
  (when-not enabledp
    (stty-exec "min" "1")))

(defn stty-change-key-stroke-signals [enabledp]
  (stty-exec "intr" (if enabledp "^C" "undef")))

(defn exec! [handler]
  (let [res (handler)]
    (if (not (= (res :exit) 0))
      (throw (new RuntimeException (string/trim (res :err))))
      (string/trim (res :out)))))

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

(defn -main [& args]
  (try
    (let [res (exec! stty-get-current-settings)]
      (try
        (println "hello, world")
        (exec! (fn [] stty-change-key-echo false))
        (doseq [elm (byte-seq (new java.io.BufferedReader *in*))]
          (when (= elm (int \q)) (System/exit 1))
          (println elm))
        (finally
          (exec! (fn [] (stty-set-current-settings res))))))
    (catch Exception e
      (println (.getMessage e))
      (System/exit 1))))
