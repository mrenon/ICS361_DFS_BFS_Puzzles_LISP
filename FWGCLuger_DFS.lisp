;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Mauricio Renon                                         Date: 9/15/14
;;; Course: ICS361        Assignment: 2 FWGC problem
;;; File: FWGCLuger_DFS.lisp

;;; To be ran with FWGCLuger.lisp
; Source: www.laulima.hawaii.edu

;;; This is one of the example programs from the textbook:
;;;
;;; Artificial Intelligence: 
;;; Structures and strategies for complex problem solving
;;;
;;; by George F. Luger and William A. Stubblefield
;;;
;;; Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
;;; 
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

;;; This file contains the depth first search algorithm from chapter 7.

; This version of depth first search does not use open and closed lists
; to keep track of states.  Instead, it uses recursion to manage the search.

;;; It takes as arguments a start state, a goal state, and a list of
;;; move functions.

;;; For example, to run depth first search with the farmer, wolf, 
;;; goat, etc. problem, evaluate the definitions found in the file 
;;; farmer_wolf_etc_rules_only, and evaluate:
;
;     (run-depth '(e e e e) '(w w w w)
;   '(farmer-takes-self farmer-takes-wolf 
;     farmer-takes-goat farmer-takes-cabbage))       
;


(defun depth-first-search (start goal been-list moves)
  (cond ((equal start goal) ;checks if the start and goal states are the same
         (reverse (cons start been-list)))
        (t (try-moves start goal been-list moves moves))))

; Try-moves scans down the list of moves in moves-to-try, 
; attempting to generate a child state.  If it produces 
; this state, it calls depth-first-search to complete the search.

(defun try-moves (start goal been-list moves-to-try moves)
  (cond ((null moves-to-try) nil) ;checks if null
        ((member start been-list :test #'equal) nil)
        (t (let ((child (funcall (car moves-to-try) start)))
             (if child 
               (or (depth-first-search (funcall (car moves-to-try) start)
                                       goal
                                       (cons start been-list)
                                       moves)
                   (try-moves start goal been-list (cdr moves-to-try) moves))
               (try-moves start goal been-list (cdr moves-to-try) moves))))))

; run-depth-first calls depth-first-search, initializing the been-list to ().
(defun run-depth (start goal moves)
  (depth-first-search start goal () moves))
