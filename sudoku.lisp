(defconstant +size+ 9   "Width and Height of the SuDoku board")
(defconstant +box-size+   3   "Width and height of the 3x3 box")
(defconstant +empty+      0   "Empty cell marker")
(defparameter alphabet (make-array 9 :initial-contents 
				   '(A B C D E F G H I)))
(defvar *grid*)
(defparameter CLV (make-array 3))

(defun modif-g (grid)
  (setf (aref grid (aref CLV 1) (aref CLV 0)) (aref CLV 2)))

(defun grid(cont)
  (defvar *nom* (make-array '(9 9) :initial-element cont)))

(defun create()
  (setf *grid* (make-array '(9 9) :initial-contents 
			   '((1 0 0 0 0 4 0 0 5)
			     (0 0 0 9 5 0 0 8 0)
			     (0 0 0 0 0 3 0 9 0)
			     (0 0 5 0 0 2 0 0 4)
			     (0 0 1 0 6 0 7 0 0)
			     (7 0 0 3 0 0 2 0 0)
			     (0 6 0 5 0 0 0 0 0)
			     (0 8 0 0 1 6 0 0 0)
			     (5 0 0 2 0 0 0 0 7)))))
									

(defun complete(grid)
    (dotimes (i +size+ t)
    (dotimes (j +size+)
      (when (zerop (aref grid i j))
        (return-from complete)))))
	       

	
(defun ask()
  (format t "~%~%C L ?")
  (let ((c (read))
	(l (read)))

   (block convertion 
     (dotimes (i +size+)
      (when (equal c (aref alphabet i))
	(setf (aref CLV 0) i)
	(return-from convertion))))
      
    (setf (aref CLV 1) l))
  (format t "Valeur ?")
  (let ((v (read)))
    (setf (aref CLV 2) v)))
  
(defun show-grid(grid)
  (format t "  | A  B  C | D  E  F | G  H  I |~%")
  (loop for i from 0 to 8
       do
       (loop for j from 0 to 8
	    do
	      (if (and (< 0 j) (zerop (mod j 3)))
		  (format t "|"))
	      
	      (if (zerop (mod j 9))
		  (format t "|~%"))
	      (if (and (zerop (mod i 3)) (zerop j))
		  (format t "**********************************~%"))
	      (if (zerop j)
		  (format t "~A |" i))
	    
	      (if (zerop (aref grid i j))
		  (format t "   ")
		 (format t " ~A " (aref grid i j)))
	      )))

  
(defun play (grid)
  (ask)
  (if (not (complete grid))      ;; Mettre fonctionc check ici !!
      (modif-g grid)
      (format t "Action impossible")))

      

(defun Sudoku (grid)
  (create)
  (loop while (not (complete grid))
       do
       (show-grid grid)
       (play grid)))
  
