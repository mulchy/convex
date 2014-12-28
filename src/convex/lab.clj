(in-ns 'convex.core)
(require '[clojure.java.io :as io])

(def data (with-open [rdr (io/reader "/home/mulchy/Documents/convex/resources/pairs.txt")] 
            (reduce conj [] (line-seq rdr))))

