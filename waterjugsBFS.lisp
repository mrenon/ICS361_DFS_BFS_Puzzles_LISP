;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                         Date: 9/15/14
;;; Course: ICS361        Assignment: 2 Water Jug Puzzle
;;; File: waterjugsBFS.lisp

; Source: LazyToad.com/lti/ai/hw1-1.html
;;; Solve by breadth-first search
;;; To be ran with waterjugs.lisp

(defun bfs (state limit)
    (setf *nodes* 0) ;set global variable nodes to 0
    (setf *expanded* 0) ;set global variable expanded to 0
    (setf *branches* 0) ;set global variable branches to 0
    (setf *limit* limit) ;set global variable limit to limit
    (setf *result* (bfs1 (list (list state))))
    (print (list *nodes* *expanded* *branches*))
    (reverse *result*))

;;; This is the Breadth First Search that relies on a queue to search nodes
(defun bfs1 (queue)
    (setf *nodes* (+ 1 *nodes*))
    (cond
	((null queue) nil)
	((goalp (caar queue)) (car queue)) ;checks if the state is a goal
	((> *nodes* *limit*) nil)
	((let ((children (new-states (caar queue))))
	     (setf *expanded* (+ 1 *expanded*))
	     (setf *branches* (+ (length children) *branches*))
	     (bfs1
		 (append
		     (cdr queue)
		     (mapcar
			 #'(lambda (state)
			       (cons state (car queue)))
			 children)))))))