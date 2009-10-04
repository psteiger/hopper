(ns hophop.algorithms.dijkstra.util
  (:use [hophop.algorithms.dijkstra data]))

(defn distance
  "Returns node's distance in dist-map"
  [node]
  (second (node @dist-map)))

(defn neighbors
  "Returns a list of the neighbors of node in the network. No side-effects"
  [node]
  (keys (node @network)))

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
     (recur @dist-map dist)))