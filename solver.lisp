(load "maze.lisp")

(defparameter *n* 5)
(defparameter *maze* (random-maze *n* *n*)) 
(defparameter *sqs* (squares *n* *n*))
(defparameter *exit* (create-edge (list (- (maze-width *maze*) 1) (- (maze-height *maze*) 1))
				  (list (- (maze-width *maze*) 1) (maze-height *maze*))))
(defparameter *edges* (cons *exit* (maze-edges *maze*)))


(defun reachable (sq n)
     (in (create-edge sq n) *edges*))


(defun available-neighbours (sq)
  (setf nbrs (intersection *sqs* (neighbours sq) :test 'equal))
  (if nbrs
    (remove-if-not #'(lambda (n) (reachable sq n)) nbrs)
     nil))


(defun unvisited-neighbours (sq vis)
  (remove-if #'(lambda (n) (in n vis)) (available-neighbours sq)))


(defun get-val (k dict)
  (cdr (assoc k dict :test 'equalp)))

(defun set-val (k v dict)
  (setf dict (remove k dict :key #'car :test 'equalp))
  (acons k v dict))

(defun intersect-dict (s dict)
  (remove-if #'(lambda (x) (in (car x) s)) dict))


(defun djikstra ()
  
  (setf dists (pairlis *sqs* (make-list (length *sqs*) :initial-element 9999)))
  ;(setf prevs (pairlis *sqs* (make-list (length *sqs*) :initial-element 9999)))
  (setf prevs '())

  (setf Q *sqs*)
  (setf dists (set-val '(0 0) 0 dists))
  (setf S '())

  (setf i 0)
  (loop while Q
    do (setf v (caar (sort (intersect-dict S dists) #'< :key #'cdr)))
       (setf Q (remove v Q :test 'equalp))
       (setf S (cons v S))

       (incf i)
       (loop for u in (available-neighbours v)
	  do (setf alt (1+ (get-val v dists)))
	     (when (< alt (get-val u dists))
	           (progn (setf dists (set-val u alt dists))
			  (setf prevs (append prevs (list (create-edge u v)))))
			  ;(setf prevs (set-val u (list v) prevs)))
			  )))

  (return-from djikstra (list dists prevs)))

(setf out (djikstra))
(setf d (car out) p (second out))


