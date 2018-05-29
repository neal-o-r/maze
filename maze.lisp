
(defstruct Edge :A :B)
(defstruct Maze :height :width :edges)
(defparameter *dot* "o")
(defparameter *lin* "--")
(defparameter *bar* "|")
(defparameter *sp* "  ")

(defun lexsort (a b)
    (cond ((< (first a) (first b)) (list a b))
          ((> (first a) (first b)) (list b a))
          ((< (second a) (second b)) (list a b))
          ((> (second a) (second b)) (list b a))
	  (t (list a b))))

(defun repeat-string (n string)
  (format nil "~V@{~a~:*~}" n string))

(defun join (a b n)
  (if (= n 1) (concatenate 'string a b) 
    (concatenate 'string (concatenate 'string a b) (join a b (- n 1)))))

(defun create-edge (a b) (make-edge :a (first (lexsort a b)) :b (second (lexsort a b))))

(defun choice (lst)
     (nth (random (length lst)) lst))

(defun neighbours (sq)
  (setf x (first sq) y (second sq))
  (list (list (1+ x) y) (list x (1+ y)) (list (- x 1) y) (list x (- y 1))))

(defun squares (w h)
  (loop for x from 0 to (- w 1)
      append (loop for y from 0 to (- h 1)
                   collect (list x y))))


(defun random-tree (nodes)
    (setf tree '())
    (setf root (pop nodes))
    (setf frontier (list root)) 
    (loop while nodes
       do (setf node (pop frontier))
          (setf nbrs (intersection nodes (neighbours node) :test 'equal))
	  
	  (when nbrs
	        (setf nbr (choice nbrs))
		(setf tree (append tree (list (create-edge node nbr))))
		(setf nodes (remove nbr nodes))
		(setf frontier (append (list node nbr) frontier))))
    (return-from random-tree tree))
 

(defun random-maze (w h)
  (setf nodes (squares w h))
  (setf tree (random-tree nodes))
  (return-from random-maze (make-maze :height h :width w :edges tree)))

(defun in (a b)
  (> (length (member a b :test 'equalp)) 0))


(defun vert-wall (x y edges)
  (if (in (create-edge (list x y) (list (1+ x) y)) edges)
       " " *bar*))

(defun horz-wall (x y edges)
  (if (in (create-edge (list x y) (list x (1+ y))) edges)
       *sp* *lin*))

(defun remove-edge (edges e)
  (remove e edges :test 'equalp))

(defun print-maze (maze)
  (setf exit (create-edge (list (- (maze-width maze) 1) (- (maze-height maze) 1)) 
			  (list (- (maze-width maze) 2) (- (maze-height maze) 1))))
  (setf edges (remove-edge (maze-edges maze) exit))
  (setf l (join *dot* *lin* (maze-width maze)))
  (print (concatenate 'string *dot* *sp* (subseq l 0 (1- (length l)))))
  (loop for y from 0 below (maze-height maze)
     do (setf s1 (loop for x from 0 below (maze-width maze) collect (concatenate 'string *sp* (vert-wall x y edges))))
        (setf s2 (loop for x from 0 below (maze-width maze) collect (concatenate 'string (horz-wall x y edges) *dot*)))
        (print (concatenate 'string *bar* (format nil "~{~A~^~}" s1)))
        (print (concatenate 'string *dot* (format nil "~{~A~^~}" s2)))))
