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


(defun update-dists (i v dist)
  (setf (aref dists (first i) (second i)) v))

(defun get-dist (i dists)
  (aref dists (first i) (second i)))

(defun djikstra ()
  
  (setf dists (make-array (list *n* *n*) :initial-element 9999))
  (setf prevs (make-array (list *n* *n*) :initial-element 9999))
  (setf curr (list 0 0))
  (setf visited '())
  (update-dists curr 0 dists)
  (setf Q *sqs*)

  (loop while Q
     do	(remove curr Q :test 'equal)
        (cons curr visited)

	(loop for n in (unvisited-neighbours curr visited)
	     (setf alt (1+ (get-dist curr dists)))
	     (if (< alt (get-dist n dists))
	         
	       )

	      ))
	
	)


