(defconstant +grid-size+ 9)
(defconstant +box-size+ 3)
(defconstant +empty+ 0)
(defconstant +size+ 80)
(defconstant +min+ 1)
(defconstant +max+ 9)
(defparameter *solution* nil)

(defun init-standalone (grid)
  "create a grid *solution*, solve it then remove all number from the initial grid to have only the solution of the grid"
  (progn
    (setf *solution* (copy grid))
    (brute-force 0)
    (substract *solution* grid)))

(defun main-standalone ()
  "return every number/pos of every number in the grid *solution*"
  (let ((row) (col) (num))
    (loop for i from 0 to +size+
       do (progn
	    (setf row (truncate i +grid-size+)
		  col (mod i +grid-size+)
		  num (aref *solution* row col))
	    (if (not (equal +empty+ num))
		(progn
		  (setf (aref *solution* row col) +empty+)
		  (return (values row col num))))))))       

(defun copy (array)
  "copy a grid from another"
  (let ((a (make-array '(9 9) :initial-element +empty+)))
    (dotimes (i +grid-size+ a)
      (dotimes (j +grid-size+)
	(setf (aref a i j) (aref array i j))))))

(defun substract (array1 array2)
  "substract one grid from another"
  (dotimes (i +grid-size+ array1)
    (dotimes (j +grid-size+)
      (setf (aref array1 i j) (- (aref array1 i j) (aref array2 i j))))))

(defun brute-force (index)
  "Try every possibilities to solve the grid"
  (let ((row (truncate index +grid-size+))
	(col (mod index +grid-size+)))
    (if (> index +size+)
	t
	(if (eq +empty+ (aref *solution* row col))
	    (loop for i from +min+ to +max+
	       do (and (check i row col)
		       (setf (aref *solution* row col) i)
		       (brute-force (+ 1 index))
		       (return t))
	       finally
		 (and
		  (setf (aref *solution* row col) +empty+)
		  (return nil)))
	    (brute-force (1+ index))))))

(defun check (num row col)
  "check if a combinaison is possible"
  (dotimes (i +grid-size+ t)
    (and (or (eq num (aref *solution* row i))
	     (eq num (aref *solution* i col))
	     (eq num (aref *solution*
			   (+ (* (truncate row +box-size+)
				 +box-size+)
			      (truncate i +box-size+))
			   (+ (* (truncate col +box-size+)
				 +box-size+)
			      (mod i +box-size+)))))
	 (return nil))))

(defun better-ia (index)
  "solve the grid in *solution* by trying for each case, each possible number (I tried to do a smarter one, but it didn't worked as expected, this is why this one is near the same as the brute-force one)"
  (let ((row (truncate index +grid-size+))
	(col (mod index +grid-size+)))
    (if (and (> index (/ (* 2 3) +size+))
	     (eq (count-empty) 0))
	t
	(if (eq +empty+ (aref *solution* row col))
	    (loop for i in (list-num row col)
	       do (and (check i row col)
		       (progn
			 (setf (aref *solution* row col) i)
			 (and (better-ia (1+ index))
			      (return t))))
	       finally
		 (progn
		   (setf (aref *solution* row col) +empty+)
		   (return nil)))
	    (better-ia (1+ index))))))			

(defun count-empty ()
  "count-empty places"
  (loop for i from 0 to +size+
     count (eq
	    +empty+
	    (aref *solution*
		  (truncate i +grid-size+)
		  (mod i +grid-size+)))))

(defun list-num (row col)
  "return a list of all num that can be placed at row/col"
  (loop for i from +min+ to +max+
     if (check i row col)
     collect i))

;; First, I wanted to do a strategy which works this way :
;;  (maybe check if the grid is enough filled to trying the smart way)
;;   -check for every case of the grid if there is one which can be filled (by checking if a number can't be in other case or if it is the only number that can directly put here).
;;   -if nothing is possible, try a random possible number then retry the smart way.
;;   -if something is possible, redo the smart way.
;;
;; I didn't make it in time so I tried to improve the brute-force
;; That was done pretty poorly thought
;;
;; (defun check-other (row col)
;;   "mean to check if there was a number which isn't in other places"
;;   (loop for i in (list-num row col)
;;        if (dotimes (j (1- +grid-size+) t)
;; 	     (and (or (not (if (eq col j)
;; 			       t
;; 			       (check i row j)))
;; 		      (not (if (eq row j)
;; 			       t
;; 			       (check i j col)))
;; 		      (not (if (and (eq (mod row +box-size+)
;; 					(truncate j +box-size+))
;; 				    (eq (mod col +box-size+)
;; 					(mod j +box-size+)))
;; 			       t
;; 			       (check i
;; 				      (+ (*
;; 					  (truncate row +box-size+)
;; 					  +box-size+)
;; 					 (truncate j +box-size+))
;; 				      (+ (*
;; 					  (truncate col +box-size+)
;; 					  +box-size+)
;; 					 (mod j +box-size+))))))))
;;      collect i))

;; (defun check-not-row (num row col)
;;   (eq 0 (loop for i from 0 to (1- +grid-size+)
;; 	   if (and (not (eq i col))
;; 		   (eq +empty+ (aref *solution* row i)))
;; 	   count (check num row i))))

;; (defun check-not-col (num row col)
;;   (eq 0 (loop for i from 0 to (1- +grid-size+)
;; 	   if (and (not (eq i row))
;; 		   (eq +empty+ (aref *solution* i col)))
;; 	   count (check num i col))))

;; (defun check-not-box (num row col)
;;   (let ((row-b 0)
;; 	(col-b 0))
;;     (eq 0 (loop for i from 0 to (1- +grid-size+)
;; 	     if (and (not (eq i (+ (mod row +box-size+)
;; 				   (mod col +box-size+))))
;; 			  (eq +empty+
;; 			      (aref *solution*
;; 				    (setf row-b
;; 					  (+ (*
;; 					      (truncate row
;; 							+box-size+)
;; 					      +box-size+)
;; 					     (truncate i +box-size+)))
;; 				    (setf col-b
;; 					  (+ (*
;; 					      (truncate col
;; 							+box-size+)
;; 					      +box-size+)
;; 					     (mod i +box-size+))))))
;; 			  count (check num row-b col-b)))))

;; (defun check-not (row col)
;;   "mean to check if there was a number which can't be placed in other places "
;;   (loop for i from +min+ to +max+
;;      if (or (check-not-row i row col)
;; 	    (check-not-col i row col)
;; 	    (check-not-box i row col))
;;      collect i))

;; (defun find-empty ()
;;   "find the first empty place"
;;   (let ((tmp 0))
;;     (list (truncate
;; 	   (setf tmp
;; 		 (loop for i from 0 to +size+
;; 		    if (eq +empty+
;; 			   (aref *solution*
;; 				 (truncate i +grid-size+)
;; 				 (mod i +grid-size+)))
;; 		    minimize i))
;; 	   +grid-size+)
;; 	  (mod tmp +grid-size+))))
		
      
;; (defun test-num (row col)
;;   "mean to check which num can be placed in one place"
;;   (let ((tmp 0))
;;     (if (or (eq 1 (length (setf tmp (check-other row col))))
;; 	    (eq 1 (length (setf tmp (check-not row col)))))
;; 	tmp))
;;  )

;; (defun better-ia (old)
;;   "I don't know what I expected :/"
;;   (if (eq 0 (count-empty))
;;       t
;;       (if (< 0 (loop for i from 0 to (1- +grid-size+)
;; 		  minimize (loop for j from 0 to (1- +grid-size+)
;; 			      if (eq +empty+ (aref *solution* i j)) 
;; 			      count (test-num i j))))
;; 	  (better-ia)
;; 	  (let ((tmp 0)
;; 		(row 0)
;; 		(col 0))
;; 	    (loop for i in (list-num
;; 			    (setf row (car (setf tmp (find-empty))))
;; 			    (setf col (cadr tmp)))
;; 	       do (progn
;; 		    (setf (aref *solution* row col) i)
;; 		    (and (better-ia) (return t)))
;; 	       finally
;; 		 (progn
;; 		   (setf (aref *solution* row col) +empty+)
;; 		   (return nil)))))))
