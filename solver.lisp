(load "maze.lisp")

(defparameter *n* 20)
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

(defun get-from (c plst)
  (mapcar #'car (remove-if-not #'(lambda (x) (equalp (first (cdr x)) c)) plst)))

(defun get-to (c plst)
  (mapcar #'car (remove-if-not #'(lambda (x) (equalp (first (cdr x)) c)) plst)))

(defun get-from (c plst)
  (cdar (remove-if-not #'(lambda (x) (equalp (car x) c)) plst)))

(defun path (target current p outpath)
  (if (equalp target current) (cons target outpath)
    (progn (push current outpath)
           (setf n (first (get-from current p)))
	   (path target n p outpath))))

(defun djikstra ()
 
  (setf dists (pairlis *sqs* (make-list (length *sqs*) :initial-element 9999)))
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
			  ;(setf prevs (a prevs (list (create-edge u v)))))
			  (setf prevs (set-val u (list v) prevs)))
			  )))

  (return-from djikstra (list dists prevs)))

(setf out (djikstra))
(setf d (car out) p (second out))
(defparameter *path* (path (list 0 0) (list (- *n* 1) (- *n* 1)) p '()))

(defun vert-wall-path (x y edges)
  (if (in (list x y) *path*) (if (= x 4) " o|" " o ")
    (if (in (create-edge (list x y) (list (1+ x) y)) edges)
       "   " "  |")))

(defun print-path (maze)
  (setf exit (create-edge (list (- (maze-width maze) 1) (- (maze-height maze) 1))
                          (list (- (maze-width maze) 1) (maze-height maze))))
  (setf edges (cons exit (maze-edges maze)))
  (setf l (join *dot* *lin* (maze-width maze)))
  (print (concatenate 'string *dot* *sp* (subseq l 0 (- (length l) 2))))
  (loop for y from 0 below (maze-height maze)
     do (setf s1 (loop for x from 0 below (maze-width maze) collect (vert-wall-path x y edges)))
        (setf s2 (loop for x from 0 below (maze-width maze) collect (concatenate 'string (horz-wall x y edges) *dot*)))
        (print (concatenate 'string *bar* (format nil "~{~A~^~}" s1)))
        (print (concatenate 'string *dot* (format nil "~{~A~^~}" s2)))))

