;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                         Date: 9/15/14
;;; Course: ICS361        Assignment: 2 Water Jug Puzzle
;;; File: waterjugsDFS.lisp

; Source: LazyToad.com/lti/ai/hw1-1.html

;;; Depth first search with state limit
;;; To be ran with waterjugs.lisp

(defun dfs (state depth limit)
    (setf *nodes* 0) ;set global variable nodes to 0
    (setf *expanded* 0) ;set global expanded to 0
    (setf *branches* 0) ;set global branches nodes to 0
    (setf *limit* limit) ;set global variable limit to limit
    (setf *result* (dfs1 state depth)) ;set global variable result to what dfs1 returns
    (print (list *nodes* *expanded* *branches*))
    *result*
)
;;; This is the Depth First Search 1 that expands a node and calls dfs2 to recurse 
;;; dfs1 expands a node and calls dfs2 to recurse on it
(defun dfs1 (state depth)
    (setf *nodes* (+ 1 *nodes*))
    (cond
	((goalp state) (list state))
	((zerop depth) nil)
	((> *nodes* *limit*) nil)
	((let ((children (new-states state)))
	     (setf *expanded* (+ 1 *expanded*)) ;set expanded to what expanded recurses to plus 1
	     (setf *branches* (+ (length children) *branches*)) ;sets branches varaiable to whatever branches returns, plus children
	     (let ((result (dfs2 children (- depth 1))))
		 (and result (cons state result)))))))

;;; This is the Depth First Search 2
;;; dfs2 recurses on each sibling from a single node, calling dfs1
(defun dfs2 (states depth)
    (cond
	((null states) nil)
	((dfs1 (car states) depth))
	((dfs2 (cdr states) depth))))