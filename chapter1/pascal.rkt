(define (pascal-triangle-element n) 
  (define (iter-layer element-number layer-n layer-greatest-element-number)
    (if (< 
          element-number 
          (inc layer-greatest-element-number)) 
      layer-n
      (iter-layer 
        element-number 
        (inc layer-n)
        (+ 
          layer-greatest-element-number
          (+ layer-n 2)))))
  (define (layer element-n) 
    (iter-layer element-n 0 0))
  (define on-edge 
    (or 
      (= n 0)
      (< 
        (layer (dec n))
        (layer n))
      (> 
        (layer (inc n))
        (layer n))))
  (define upper-left-element-n 
    (- 
      n 
      (inc (layer n))))
  (if on-edge
      1
      (+ 
        (pascal-triangle-element upper-left-element-n) 
        (pascal-triangle-element (inc upper-left-element-n)))))
