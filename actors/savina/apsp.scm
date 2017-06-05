(define NumNodes (int-top))
(define BlockSize (int-top))
(define GraphData ???)
(define NumBlocksInSingleDim (/ NumNodes BlockSize))

(define apsp-fw-actor
  (a/actor "apsp-fw-actor" (block-id init-graph-data k neighbors)
           ())) ;; blockSize is BlockSize, graphSize is NumNodes
