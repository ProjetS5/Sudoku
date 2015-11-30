
(defconstant board-size 9   "Width and Height of the SuDoku board")
(defconstant box-size   3   "Width and height of the 3x3 box")
(defconstant empty      0   "Empty cell marker")




(defun modif_g (grid c l new)
  (setf (aref grid l c) new))

(defun creer_grille (grid)
  (defvar *grid* (make-array '(9 9) :initial-contents 
			   '((1 0 0 0 0 4 0 0 5)
			     (0 0 0 9 5 0 0 8 0)
			     (0 0 0 0 0 3 0 9 0)
			     (0 0 5 0 0 2 0 0 4)
			     (0 0 1 0 6 0 7 0 0)
			     (7 0 0 3 0 0 2 0 0)
			     (0 6 0 5 0 0 0 0 0)
			     (0 8 0 0 1 6 0 0 0)
			     (5 0 0 2 0 0 0 0 7)))))
									

(defun Afficher_Grille (grid)
  (format t "  | A  B  C | D  E  F | G  H  I |~%")
  (loop for i from 0 to 8
       do
       (loop for j from 0 to 8
	    do
	      (if (and (< 0 j) (= 0 (mod j 3)))
		  (format t "|"))
	      
	      (if (= 0 (mod j 9))
		  (format t "|~%"))
	      (if (and (= 0 (mod i 3)) (= 0 j))
		  (format t "**********************************~%"))
	      (if (= 0 j)
		  (format t "~A |" i))
	    
	      (if (= 0 (aref grid i j))
		  (format t "   ")
		 (format t " ~A " (aref grid i j)))
	      

	      )))

  


;(defun Sudoku (grid) 
   ;(Afficher_Grille grid)

