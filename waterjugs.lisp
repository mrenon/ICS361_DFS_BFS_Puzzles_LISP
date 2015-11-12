;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                         Date: 9/15/14
;;; Course: ICS361        Assignment: 2 Water Jug Puzzle
;;; File: waterjugs.lisp

; Source: LazyToad.com/lti/ai/hw1-1.html


;;; Solve the Water Jug problem
;;; Main program to solve the water jug problem

(load "waterjugsDFS.lisp") ;loads the depth-first-search algorithm 

(load "waterjugsBFS.lisp") ;loads the breadth-first-search algorithm

(defvar *start* '(0 0))

;returns the quantity of the first water jug,
;by giving the first parameter in the list
(defun first-jug (state) (car state)) 

;returns the quantity of the second water jug
(defun second-jug (state) (cadr state))

;this returns the state of the two jugs
(defun mk-state (f s) (list f s))

;checks whether the given state is a goal
;the goal is to get 4 into the second jug
(defun goalp (state)
    (eq (first-jug state) 4))

;retuns all possible states, from the given state
(defun new-states (state)
    (remove-null
	(list
	    (fill-first state)
	    (fill-second state)
	    (pour-first-second state)
	    (pour-second-first state)
	    (empty-first state)
	    (empty-second state))))

;this removes null states
(defun remove-null (x)
    (cond
	((null x) nil)
	((null (car x)) (remove-null (cdr x)))
	((cons (car x) (remove-null (cdr x))))))

;returns the state when the first jug is filled to 3
(defun fill-first (state)
    (cond
	((< (first-jug state) 5) (mk-state 5 (second-jug state)))))) ; checks to see if the first jug state is less than 3, if it is, change the state to 3.

;returns the state when the second jug is filled to 5
(defun fill-second (state)
    (cond
	((< (second-jug state) 3) (mk-state (first-jug state) 3)))) ; checks to see if the second jug state is less than 5, if it is, change the state to 5.

;returns the state when the first jug is poured into the second
(defun pour-first-second (state)
    (let (   (f (first-jug state)) ;set f to the first jug
	     (s (second-jug state))) ;set s to the second jug
	(cond
	    ((zerop f) nil)		; Cant pour nothing
	    ((= s 3) nil)		; Second full
	    ((<= (+ f s) 3)		; Empty first into second
		(mk-state 0 (+ f s)))
	    (t				; Fill second from first
		(mk-state (- (+ f s) 3) 3)))))

;returns the state when the second jug is poured into the first
(defun pour-second-first (state)
    (let (   (f (first-jug state)) ;set f to the first jug
	     (s (second-jug state))) ;set s to the second jug
	(cond
	    ((zerop s) nil)		; Cant pour nothing
	    ((= f 5) nil)		; First full	    
	    ((<= (+ f s) 5)		; Empty second into first
		(mk-state (+ f s) 0))	    
	    (t				; Fill first from second
		(mk-state 5 (- (+ f s) 5))))))

;checks if the first jug is empty
;if the jug has anything greater than, change than jug to 0 and empty it
(defun empty-first (state)
    (cond
	((> (first-jug state) 0) (mk-state 0 (second-jug state)))))

;checks if the second jug is empty
;if the jug has anything greater than, change than jug to 0 and empty it
(defun empty-second (state)
    (cond
	((> (second-jug state) 0) (mk-state (first-jug state) 0))))