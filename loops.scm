;; XXX: loops are one of the essential things to add
(let loop ((n 0))
  (if (> n 10)
      '()
      (loop (+ n 1))))
