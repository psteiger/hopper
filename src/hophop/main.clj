(ns hophop.main
  (:use [hophop.algorithms.dijkstra main]))

(def network (ref {}))

(defn add-node
  "Adds a node (a keyword) associated with a map of targets to the network.
  The targets should be in the form:
  {:label-1 dist-1 :label-2 dist-2 ... :label-n dist-n}
  Example:
  (add-to-network :1 {:2 3 :4 5})"
  [node targets]
  (let [new-targets (merge (node @network) targets)] ; merge old targets with new targets
    (dosync
     (ref-set network (assoc @network node new-targets))))) ; associate new-targets with node

(defn add-all-nodes
  "Adds a node (keyword) associated with a map of targets to the network,
  also adding all the target nodes to the network. See: add-node"
  [node targets]
  (add-node node targets) ; add actual node and targets to network
  (loop [loop-targets targets t (first targets)] ; loop over targets in target-list
    (if (not (nil? t))
      (let [newnode (first t)
	    dist    (second t)]
	(add-node newnode {node dist}) ; add target node with target {node dist}
	(recur (rest loop-targets) (first (rest loop-targets)))))))

(add-all-nodes :1 {:2 3 :4 5})
(dijkstra :1)