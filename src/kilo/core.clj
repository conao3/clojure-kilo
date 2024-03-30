(ns kilo.core
  (:gen-class))

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
  (println "hello, world")
  (doseq [elm (byte-seq (new java.io.BufferedReader *in*))]
    (when (= elm (int \q)) (System/exit 1))
    (println elm)))
