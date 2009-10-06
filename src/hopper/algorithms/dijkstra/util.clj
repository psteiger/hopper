(ns hopper.algorithms.dijkstra.util
  (:use [hopper.algorithms.dijkstra data]
	[hopper                     data]))

(defn distance
  "Returns node's distance in dist-map"
  [node]
  (second (node @dist-map)))

(defn dist-between
  [u v]
  (u (v @network)))

(defn key-by-dist
  "Returns the key based on the dist value"
  ([argmap dist]
     (let [current-key (key (first argmap))]
       (cond (empty? argmap) nil
	     (= dist (distance current-key)) current-key
	     :else (recur (rest argmap) dist))))
  ([dist]
     (key-by-dist @dist-map dist)))
