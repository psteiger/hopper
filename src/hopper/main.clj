(ns hopper.main
  (:use [hopper.algorithms dijkstra]
	[hopper util]))

(add-all-nodes :1 {:2 3 :4 5 :3 6})
(add-all-nodes :4 {:2 1 :3 2})
(prn (update-dist :1 :4))
