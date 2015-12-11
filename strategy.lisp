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
    (better-ia 0)
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
