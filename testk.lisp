#|
nix-shell -p tk rlwrap lispPackages.quicklisp --command "rlwrap quicklisp run -- --load 'testk.lisp' "
exit


echo exec rlwrap quicklisp run -- --load $@
exit
|#

;; (ql:quickload :ltk)
(require :ltk)

(defpackage #:testk
  (:use :cl))

(in-package #:testk)

(defparameter *tests* (make-hash-table))

#|
(defun make-test (package body)
  (list package body))

(defmacro deftest (name &body body)
  (check-type name symbol)
  `(setf (gethash ',name *tests*) (make-test *package* '(progn ,@body))))

(defun run-test (name)
  (destructuring-bind (package body)
      (gethash name *tests*)
    (let ((passed nil)
          (condition nil))
      (with-output-to-string (*standard-output*)
        (handler-case (progn
                        (let ((*package* package))
                          (eval body))
                        (setf passed t))
          (error (c) (setf condition c))))
      (list passed condition))))

(defmacro is (&body body)
  `(unless (progn ,@body)
     (error "Expression is falsy: ~A" '(progn ,@body))))

;; This is ok, but it doesn't have a clear report
(defun run-all-tests ()
  (loop :for name :being :the :hash-keys :of *tests*
        :collect (list name (run-test name))))
|#

(defun start-ui ()
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* "Tests")
    (let ((treeview (make-instance 'ltk:treeview)))
      (loop :for i :below 50
            :do (ltk:treeview-insert treeview :text (prin1-to-string i)))
      (ltk:pack treeview :expand :ll))))

(start-ui)

