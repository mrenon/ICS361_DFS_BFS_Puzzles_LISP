;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                         Date: 9/15/14
;;; Course: ICS361        Assignment: 2 FWGC problem
;;; File: FWGCLuger_BFS.lisp

;;; To be ran with FWGCLuger.lisp
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

;;; This program defines a more sophisticated version of breadth first search.
;;; on finding a solution, it uses a record of each state's parents to print 
;;; the path to the goal.  It is discussed in chapter 7 of the text.
;;;
;;; For example, to run it on the farmer, wolf, goat, etc. problem,
;;; evaluate the definitions of move rules found in the file:
;;;      farmer_wolf-etc-rules-only.lisp
;;;
;;; and then evaluate
;;;
;;; (run-breadth '(e e e e) '(w w w w)
;;;             '(farmer-takes-self farmer-takes-wolf
;;;              farmer-takes-goat farmer-takes-cabbage))


(defun run-breadth (start goal moves)
  (declare (*open*)) ;declare global variable open
  (declare (*closed*)) ;global variable closed
  (declare (*goal*)) ;global variable goal
  (setq *open* (list (build-record start nil))) ;sets open to a list of the return of build-record
  (setq *closed* nil) ;set closed to nil
  (setq *goal* goal) ;set goal to goal
  (breadth-first moves))

;;; These functions handle the creation and access of (state parent) 
;;; pairs.

(defun build-record (state parent) (list state parent)) ;returns the parent list

(defun get-state (state-tuple) (nth 0 state-tuple)) ;gets the state of the first (0) tuple

(defun get-parent (state-tuple) (nth 1 state-tuple)) ;gets the parent or 1st tuple


(defun retrieve-by-state (state list)
  (cond ((null list) nil)
        ((equal state (get-state (car list))) (car list))
        (t (retrieve-by-state state (cdr list)))))



(defun breadth-first (moves)
  (declare (*open*)) ;global variable open
  (declare (*closed*)) ;global variable closed 
  (declare (*goal*)) ;global variable goal
  (cond ((null *open*) nil) 
        (t (let ((state (car *open*))) ;sets state to the return of the first element
             (setq *closed* (cons state *closed*)) ;sets closed

             (cond 
  ;;; found solution: print path to it
      ((equal (get-state state) *goal*) (reverse (build-solution *goal*)))
             
            ;;; try next child state
                (t (setq *open* 
                            (append (cdr *open*)
                                    (generate-descendants (get-state state)
                                                          moves)))
                      (breadth-first moves)))))))

(defun generate-descendants (state moves)
  (declare (*open*)) ;global variable open
  (declare (*closed*)) ;global variable closed
  (cond ((null moves) nil) ;checks if null
        (t (let ((child (funcall (car moves) state))
                 (rest (generate-descendants state (cdr moves))))
             (cond ((null child) rest) ;checks if the child is null
                   ((retrieve-by-state child rest) rest) ;continue
                   ((retrieve-by-state child *open*) rest)
                   ((retrieve-by-state child *closed*) rest)
                   (t (cons (build-record child state) rest)))))))


(defun build-solution (state) ;returns the solution
  (declare (*closed*)) ;global variable closed
  (cond ((null state) nil) ;checks if null
        (t (cons state (build-solution 
                        (get-parent 
                         (retrieve-by-state state *closed*)))))))