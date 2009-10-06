(ns hopper.algorithms.dijkstra
  (:use 
   [hopper util data]
   [hopper.algorithms.dijkstra util data]))

(defn initialize-dist-map
  [source]
  (dosync (ref-set dist-map {}))
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
  "Updates the status of a node's neighbors within network.
  A updated key in the dist-map looks like:
  :neighbor [n d], where:
  neighbor is a neighbor of node, 
  n is the node of origin, and
  d is the distance between node and neighbor"
  [node]
  (dorun
   (map #(if (not (contains? @processed %1))
	   (update-node node %1))
	(neighbors node))))

(defn add-to-processed
  [node]
  (dosync
   (ref-set processed (assoc @processed node nil))))

(defn unprocessed
  "Returns the neighbors of node of origin that can be processed
  in the Dijkstra algorithm (i.e. were not yet processed)"
  [source]
  (let [processable (filter #(not (contains? @processed %1)) 
			    (neighbors source))]
  (keys (select-keys @dist-map processable))))

(defn nearest-neighbor
  [source]
  (prn "min " (map distance (keys (select-keys @dist-map (filter #(not (contains? @processed %1)) (neighbors source))))))
  (let [min-dist (apply min (map distance (unprocessed source)))]
    (key-by-dist min-dist)))

(defn reset-all
  []
  (dosync
   (ref-set processed {})
   (ref-set dist-map {})))

(defn done-dijkstra
  []
  (or (= (count @processed) (dec (count @dist-map))))) 

(defn update-dist
  "Updates the dist-map through the Dijkstra Algorithms"
  ([source]
     (initialize-dist-map source)
     (loop [processing source]
       (if (nil? processing)
	 @dist-map
	 (do
	   (update-neighbors processing)
	   (add-to-processed processing)
	   (if (done-dijkstra)
	     @dist-map
	     (recur (nearest-neighbor processing)))))))
  ([source dest] ; 1 4
     (reset-all)
     (update-dist source)
     (prn @dist-map (first (dest @dist-map)))
     (cons dest ; 4
	   (if (not= source (first (dest @dist-map)))
	     (update-dist source (first (dest @dist-map)))
	     (cons source '())))))	       
