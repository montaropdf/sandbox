;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               main.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;; File originally generated with Emacs template found at
;;;; 'https://github.com/montaropdf/reve-workshop/elisp/'.
;;;;
;;;;AUTHORS
;;;;    <> Roland Everaert <roland@tanko>
;;;;MODIFICATIONS
;;;;    YYYY-MM-DD <> Some comment describing a modification.
;;;;BUGS
;;;; 
;;;;LEGAL
;;;;
;;;; Copyright (C) 2020 by Roland Everaert
;;;;
;;;;****************************************************************************
(defpackage main
  (:use :cl)
;;; Export functions
  ;;
  ;; (:export "awesome-function-of-doom" "terrifying-macro-of-courtesy")
  ;;
;;; Import functions
  ;;
  ;; (:import-from "that.great.package.extra" "another-awesome-function" "that-great-function-i-like-to-use-in-every-file")
  (:documentation
   "Testing tree building and traversal"))

(in-package :main)

;;; Begin to write your code here.

(defparameter *a-tree* '((a (0 1) (a r)) (b (4r (var #\%)))))

(defparameter *a-path* '(b 4r var))

(defparameter *wrong-path* '(a var))

(defun walk-dolist (path tree)
  "Walk in the TREE following PATH and spit the element found or nil.

This function use dolist to wlk through the node at the same level."
  (let ((spit nil)
        (node-h nil))
    (format t "PATH: ~a / TREE: ~a / CAR-TREE: ~a / CDR-TREE ~a~%" path tree (car tree) (cdr tree))
    ;; Loop over each node at the same level
    (dolist (node tree spit)
      (format t "NODE: ~a~%" node)
      ;; Is the node a cons or a symbol?
      (unless (setf node-h (if (consp node) (car node)
                               (if (symbolp node) node)))
        (error "Bad node type: ~a / ~a" (type-of node) node))
      ;; Is the node in the path?
      (when (eq (car path) node-h)
        (format t "PATH-FOUND: ~a ~a~%" (car path) (cdr path))
        ;; Do we need to go deeper in the branches
        (if (cdr path)
            ;; Yes, check for the next element of the path 
            (setf spit (walk-dolist (cdr path) (cdr node)))
            ;; No, return the tip of the branch, the leaf
            (setf spit (cdr node)))
        (format t "SPIT: ~a~%" spit)
        (return)))
    (if (eq (length spit) 1) (car spit) spit)))


(defun walk-cdr (path tree)
  "Walk in the TREE following PATH and sweat the element found or nil.

This function does not use any loop construct to walk through the node
at the same level. It calls itself with the CDR of TREE."
  (let ((sweat nil)
        (breathe (car tree))
        (node-h nil))
    (format t "PATH: ~a / TREE: ~a / CAR-TREE: ~a / CDR-TREE ~a~%" path tree (car tree) (cdr tree))
    ;; Is the exhaled node a cons or a symbol?
    (unless (setf node-h (if (consp breathe) (car breathe)
                             (when (symbolp breathe) breathe)))
      (error "Bad node type: ~a / ~a" (type-of breathe) breathe))
    ;; Is inhaling keep us on track?
    (if (eq (car path) node-h)
        ;; Yes, check if we finish walking
        (progn
          (format t "PATH-FOUND: ~a ~a~%" (car path) (cdr path))
          ;; Do we need to continue?
          (if (cdr path)
              ;; Yes, continue on this path
              (setf sweat (walk-cdr (cdr path) (cdr breathe)))
              ;; No, time to sweat off
              (setf sweat (cdr breathe))))
        ;; No, time to exhale and keep walking
        (progn
          (format t "PATH-NOT-FOUND: trying with next breathe!~%")
          (format t "Remainder: ~a~%" (cdr tree))
          ;; If not at the end of the run...
          (when (cdr tree)
            ;; continue with a side track
            (setf sweat (walk-cdr path (cdr tree))))))
    (format t "SWEAT: ~a~%" sweat)
    (if (eq (length sweat) 1) (car sweat) sweat)))

;;; Code ends here.
