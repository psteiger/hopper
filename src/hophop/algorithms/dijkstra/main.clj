(ns hophop.algorithms.dijkstra.main
  (:use [hophop.algorithms.dijkstra util data]))

(defn initialize-dist-map
  [source]
  (dorun ; maps are lazy, so dorun is needed
   (map #(dosync
	  (if (= %1 source)
	    (ref-set dist-map (assoc @dist-map %1 [source 0]))
	    (ref-set dist-map (assoc @dist-map %1 [nil    Double/POSITIVE_INFINITY]))))
	(keys @network))))

(defn update-node
  [source node]
  (if (< (dist-between source node) (distance node))
    (dosync
     (ref-set dist-map (assoc @dist-map node [source (dist-between source node)])))))

(defn update-neighbors
  "Updates the status of neighbors within network.
  A updated key in the dist-map looks like:
  :neighbor [n d], where:
  neighbor is a neighbor of node, 
  n is the node, and
  d is the distance within node and neighbor"
  [node]
  (dorun
   (map #(if (not (contains? @processed %1))
	   (update-node node %1))
	(neighbors node))))

(defn add-to-processed
  [node]
  (dosync
   (ref-set processed (assoc @processed node nil))))

(defn nearest-neighbor
  [source]
  (let [processable (filter #(not (contains? @processed %1))
			    (neighbors source))
	unprocessed (keys (select-keys @dist-map processable))
	min-dist    (apply min (map distance unprocessed))]
    (key-by-dist min-dist)))
 
(defn dijkstra
  [source]
  (initialize-dist-map source)
  (loop [processing source]
    (if (nil? processing)
      @dist-map
      (do
	(update-neighbors processing)
	(add-to-processed processing)
	(if (= (count @processed) (count @dist-map))
	  @dist-map
	  (recur (nearest-neighbor processing)))))))