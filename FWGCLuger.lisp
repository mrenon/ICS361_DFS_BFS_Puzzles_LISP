;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                         Date: 9/15/14
;;; Course: ICS361        Assignment: 2 FWGC problem
;;; File: FWGCLuger.lisp

; Source: www.laulima.hawaii.edu


;;; This is one of the example programs from the textbook:
;;;
;;; Artificial Intelligence: 
;;; Structures and strategies for complex problem solving
;;;
;;; by George F. Luger and William A. Stubblefield
;;; 
;;; These programs are copyrighted by Benjamin/Cummings Publishers.
;;;
;;; We offer them for use, free of charge, for educational purposes only.
;;;
;;; Disclaimer: These programs are provided with no warranty whatsoever as to
;;; their correctness, reliability, or any other property.  We have written 
;;; them for specific educational purposes, and have made no effort
;;; to produce commercial quality computer programs.  Please do not expect 
;;; more of them then we have intended.
;;;
;;;
;;; This code has been tested with CMU Common Lisp CVS release-19a
;;; 19a-release-20040728 and appears to function as intended.



(load "FWGCLuger_BFS.lisp") ;loads the Breadth First Search from the book to be used with this code
(load "FWGCLuger_DFS.lisp") ;loads the Depth First Search from the book to be used with this code

;;; This is the farmer, wolf, goat and cabbage problem from section 7.2 
;;; of the text.
;;; solve-fwgc initiates the search.  A typical starting function call 
;;; might be:
;;;
;;;                   (solve-fwgc '(e e e e) '(w w w w))
;;;
;;; This finds a path from the east bank to the west.


(defun solve-fwgc (state goal) (path state goal nil))


;;; The recursive path algorithm searches the space in a depth first 
;;; fashion.

(defun path (state goal been-list)
   (cond ((null state) nil)
	 ((equal state goal) (reverse (cons state been-list)))
	 ((not (member state been-list :test #'equal))
	      (or (path (farmer-takes-self state) goal (cons state been-list))
	          (path (farmer-takes-wolf state) goal (cons state been-list))
	          (path (farmer-takes-goat state) goal (cons state been-list))
	          (path (farmer-takes-cabbage state) goal (cons state been-list))))))


;;; These functions define legal moves in the state space.  They take
;;; a state as argument, and return the state produced by that operation.

;;the farmer takes himself across the river flips his state
;;the other entities states stay the same (wolf, goat cabbage)
(defun farmer-takes-self (state)
   (safe (make-state (opposite (farmer-side state))
		(wolf-side state)
	 	(goat-side state)
	 	(cabbage-side state))))

;;the farmer takes himself and the wolf across the river flips their state
;;the other entities states stay the same (goat, cabbage)
(defun farmer-takes-wolf (state)
   (cond ((equal (farmer-side state) (wolf-side state))
                     (safe (make-state (opposite (farmer-side state))
	                                        (opposite (wolf-side state))
	                                        (goat-side state)
	                                        (cabbage-side state))))
   	    (t nil)))

;;the farmer takes himself and the goat across the river flips their state
;;the other entities states stay the same (wolf, cabbage)
(defun farmer-takes-goat (state)
   (cond ((equal (farmer-side state) (goat-side state))
                  (safe (make-state (opposite (farmer-side state))
	                                     (wolf-side state)
	                                     (opposite (goat-side state))
	                                     (cabbage-side state)))) 
  	    (t nil)))

;;the farmer takes himself and the cabbage across the river flips their state
;;the other entities states stay the same (wolf, goat)
(defun farmer-takes-cabbage (state)
   (cond ((equal (farmer-side state) (cabbage-side state))
                    (safe (make-state (opposite (farmer-side state))
	                                       (wolf-side state)
	                                       (goat-side state)
	                                       (opposite (cabbage-side state)))))   
	   (t nil)))



;;; These functions define states of the world
;;; as an abstract data type.

(defun make-state (f w g c) (list f w g c))

(defun farmer-side ( state ) ;grabs the first element on the list (farmer) and tells the state
   (nth 0 state))

(defun wolf-side ( state ) ;grabs the second element on the list (wolf) and tells the state
   (nth 1 state))

(defun goat-side ( state ) ;grabs the third element on the list (goat) and tells the state
   (nth 2 state)) 

(defun cabbage-side ( state ) ;grabs the fourth element on the list (cabbage) and tells the state
   (nth 3 state))

;;; The function "opposite" takes a side and returns the opposite
;;; side of the river.

(defun opposite (side)
   (cond ((equal side 'e) 'w) ;this is the function that flips the states of the entities
             ((equal side 'w) 'e)))

;;; Safe returns nil if a state is not safe; it returns the state unchanged
;;; if it is safe.

(defun safe (state)
   (cond ((and (equal (goat-side state) (wolf-side state)) ;checks to see if the goat and wolf are on the same side
	             (not (equal (farmer-side state) (wolf-side state))))  nil)
            ((and (equal (goat-side state) (cabbage-side state)) ;checks to see if the goat and the cabbage are on the same side
	             (not (equal (farmer-side state) (goat-side state)))) nil)
	   (t state)))

  

