;;; package -- breeze integration with emacs

;;; Commentary:
;;
;; Features:
;; - snippets with abbrevs
;; - capture
;; - interactive make-project

;;; Code:

;;; Scratch section
;; (defun breeze-eval-defun )


;;; Requires
(require 'cl)
(require 'evil) ;; Actually optional

;;; Groups and customs
(defgroup breeze nil
  "breeze")

(defcustom breeze-default-author ""
  "The default author when generating asdf system."
  :type 'string
  :group 'breeze)

(defcustom breeze-default-licence "GNU GPLv3"
  "The default licence when generating asdf system."
  :type 'string
  :group 'breeze)

(defcustom breeze-capture-folder "~/breeze-capture"
  "The folder where to save scratch files."
  :type 'string
  :group 'breeze)

(defcustom breeze-capture-template
"

(ql:quickload '(alexandria))

;; Make it easier to debug
(declaim (optimize (speed 0) (safety 3) (debug 3)))

#|

Goal:

Motivation:

First lead:

|#

"
  "The fixed template to insert in a new scratch file."
  :type 'string
  :group 'breeze)



;;; Variables

(defvar breeze-mode-map (make-sparse-keymap))


;;; Utilities

(cl-defun breeze-choose-and-call-command (prompt commands &key allow-other-p)
  "Let the user choose a command to run from a list."
  (let* ((choice (completing-read prompt (mapcar 'car commands)))
	 (command (or (cadr (assoc choice commands))
		      (and allow-other-p choice))))
    (when command
      (call-interactively command))))

(defun breeze-contains-space-or-capital (str)
  "Return nil if STR does not contain space or capital letters.
Othewise, return the position of the character."
  (let ((case-fold-search nil)) ;; make it case-sensitive
    (string-match "[[:blank:][:upper:]]" str)))

(defun breeze-sanitize-symbol (name)
  "Wrap a symbol NAME with pipe \"|\" symbol, if needed."
  (if (breeze-contains-space-or-capital name)
      (concat "|" name "|")
    name))


;;; Insertion commands (mostly skeletons)

(define-skeleton breeze-insert-defpackage
  "Skeleton for a CL package"
  "" ;; Empty prompt. Ignored.
  (let ((package-name (breeze-sanitize-symbol
		       (skeleton-read "Package name: "
				      (file-name-base buffer-file-name))))
	(nicknames (loop for nickname = (skeleton-read "Package nickname: ")
			 while (not (zerop (length nickname)))
			 collect (format ":%s" nickname))))
    (concat
     "(in-package #:common-lisp-user)\n\n"
     "(defpackage #:" package-name "\n"
     (when nicknames
       (concat "  " (prin1-to-string `(:nicknames ,@nicknames)) "\n"))
     "  (:use :cl))\n\n"
     "(in-package #:" package-name ")\n\n")))

(define-skeleton breeze-insert-defun
  "Insert a defun form."
  "" ;; Empty prompt. Ignored.
  > "(defun " (skeleton-read "Function name: ") " (" ("Enter an argument name: " str " ") (fixup-whitespace) ")" \n
  > _ ")")

(define-skeleton breeze-insert-defmacro
  "Insert a defvar form."
  "" ;; Empty prompt. Ignored.
  > "(defmacro " (skeleton-read "Function name: ") " (" ("Enter an argument name: " str " ") (fixup-whitespace) ")" \n
  > _ ")")

(define-skeleton breeze-insert-defvar
  "Insert a defvar form."
  "" ;; Empty prompt. Ignored.
  > "(defvar *" (skeleton-read "Name: ") "* " (skeleton-read "Initial value: ") \n
  > "\"" (skeleton-read "Documentation string: ") "\")")

(define-skeleton breeze-insert-defparameter
  "Insert a defparameter form."
  "" ;; Empty prompt. Ignored.
  > "(defparameter *" (skeleton-read "Name: ") "* " (skeleton-read "Initial value: ") \n
  > "\"" (skeleton-read "Documentation string: ") "\")")

(define-skeleton breeze-insert-let
  "Insert a let defvar form."
  "" ;; Empty prompt. Ignored.
  > "(let ((" @ ")))")

(define-skeleton breeze-insert-asdf
  "Skeleton for an asdf file."
  "" ;; Empty prompt. Ignored.
  "(defpackage #:" (setq v1 (skeleton-read "System name: ")) ".asd" \n
  > "(:use :cl :asdf))" \n
  \n
  > "(in-package #:" v1 ".asd)" \n
  \n
  > "(asdf:defsystem \"" v1 "\"" \n
  > ":description \"\"" \n
  > ":version \"0.1.0\"" \n
  > ":author \"" (skeleton-read "Author name: " breeze-default-author) "\"" \n
  > ":licence \"" (skeleton-read "Licence name: " breeze-default-licence) "\"" \n
  > ":depends-on ()" \n
  > ":serial t" \n
  > ":components" \n
  > "((:module \"src\"" \n
  > ":components ())" \n
  > "(:module \"tests\"" \n
  > ":components ())))")

(define-skeleton breeze-insert-loop-clause-for-hash
  "Skeleton to insert a loop clause to iterate on a hash-table."
  "" ;; Empty prompt. Ignored.
  > " :for "
  (skeleton-read "Enter the variable name for the key: ")
  " :being :the :hash-key :of "
  (skeleton-read "Enter the name of the hash-table: ")
  " :using (hash-value "
  (skeleton-read "Enter the variable name for the value: ")
  ")")

(defun breeze-insert ()
  "Choose someting to insert."
  (interactive)
  ;; TODO filter the choices based on the context
  (breeze-choose-and-call-command
   "What do you want to insert? "
   '(("asdf:defsystem" breeze-insert-asdf)
     ("defpackage" breeze-insert-defpackage)
     ("defun" breeze-insert-defun)
     ("defvar" breeze-insert-defvar)
     ("defparameter" breeze-insert-defparameter)
     ("let" breeze-insert-let)
     ("defmacro" breeze-insert-defmacro)
     ("loop clause: hash-table iteration" breeze-insert-loop-clause-for-hash)
     ;; TODO
     ;; defclass
     ;; slots
     ;; defgeneric
     ;; defmethod
     )))


;;; abbrev

;; P.S. This doesn't work anymore because (I think) breeze-mode is a
;; minor mode.
(progn
  (when (boundp 'breeze-mode-abbrev-table)
    (clear-abbrev-table breeze-mode-abbrev-table))
  (define-abbrev-table 'breeze-mode-abbrev-table
    '(("defmacro1" "" breeze-insert-defmacro)
      ("defpackage1" "" breeze-insert-defpackage)
      ("defparam1" "" breeze-insert-defparameter)
      ("defsystem1" "" breeze-insert-asdf)
      ("defun1" "" breeze-insert-defun)
      ("defvar1" "" breeze-insert-defvar))))


;;; code modification

;; I discovered that Paredit has M-q that does it better :/
(defun breeze-indent-defun-at-point ()
  "Indent the whole form without moving."
  (interactive)
  (if (zerop (current-column))
      (indent-sexp)
    (save-excursion
      (slime-beginning-of-defun)
      (indent-sexp))))

(defun breeze-get-symbol-package (symbol)
  "SYMBOL must be a string.  Return a list with the package name and the symbol name."
  (cl-destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output
		    ,(format (format "%s"
				     `(let ((symbol (quote %s)))
					(check-type symbol symbol)
					(format t "\"~(~a ~a~)\""
						(package-name (symbol-package symbol))
						(symbol-name symbol))))
			     symbol)))

    (split-string output)))

;; (breeze-get-symbol-package "cl:find")
;; => ("common-lisp" "find")

(defun breeeze-import-from ()
  "Add or update defpackage to \"import-from\" the symbol at point.

   TODO it assumes there's a defpackage form in the current
   buffer, that it has an exisint \"import-from\" from and that
   paredit-mode is enabled."
  (interactive)
  (let ((symbol (slime-symbol-at-point))
	(case-fold-search t)) ;; case insensitive search
    (destructuring-bind (package-name symbol-name)
	(breeze-get-symbol-package symbol)
      (message "%s:%s" package-name symbol-name)
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (concat "import-from[[:space:]:#]+" package-name))
	    (progn
	      (insert (format "\n#:%s" symbol-name))
	      (slime-beginning-of-defun)
	      (indent-sexp)
	      (slime-eval-defun))
	  ;; TODO else, search for defpackage, add it at the end
	  )))))

;; (slime-goto-package-source-definition "breeze")
;; (slime-goto-xref)

;; (slime-rex (var ...) (sexp &optional package thread) clauses ...)

;; (slime-interactive-eval "(breeze.swank:)")

;; (global-set-key
;;  (kbd "<f5>")
;;  (lambda ()
;;    (interactive)
;;    (slime-interactive-eval
;;     (concat "(breeze.swank::insert-let "
;; 	    (replace-match "\\\""  "fixedcase" "literal")
;; 	    (slime-defun-at-point)
;; 	    "4"
;; 	    ")"))))

;; Idea: (defun breeze-wrap-with-let-form ())

(defun breeze-move-form-into-let ()
  "Move the current form into the nearest parent \"let\" form."
  (interactive)
  (let* ((form (slime-sexp-at-point-or-error))
	 (new-variable)
	 ;; Find the position of the top-level form
	 (beginning-of-defun (if (zerop (current-column))
				 (point)
			       (save-excursion
				 (slime-beginning-of-defun)
				 (point))))
	 (case-fold-search t) ;; case insensitive search
	 ;; Find the position of the previous "let"
	 (let-point (save-excursion (re-search-backward "let"))))
    (save-excursion
      ;; Check if we found a parent let form
      (if (and let-point
	       (>= let-point beginning-of-defun))
	  (progn
	    (save-excursion
	      ;; Find the place to add the new binding
	      (re-search-backward "let")
	      (re-search-forward "(")
	      (save-excursion
		(insert (format "( %s)" form))
		;; Add a newline if necessary
		(if (eq (char-after) (string-to-char "("))
		    ;; also add a space to help indentation later
		    (insert "\n ")))
	      (forward-char)
	      ;; Ask the user to name the new-variable
	      (setq new-variable (read-string "Enter a new for the new-variable: "))
	      ;; Insert the name of the variable in the let form.
	      (insert new-variable))
	    ;; Replace the form at point by the new variable
	    (let ((start (point)))
	      (forward-sexp)
	      (delete-region start (point)))
	    (insert new-variable)
	    ;; reindent the whole top-level form
	    (slime-beginning-of-defun)
	    (indent-sexp))
	(message "Failed to find a parent let form.")))))


;;; code evaluation

(defun breeze-get-recently-evaluated-forms ()
  "Get recently evaluated forms from the server."
  (cl-destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output
		    "(breeze.swank:get-recent-interactively-evaluated-forms)"))
    (split-string output "\n")))

(defun breeze-reevaluate-form ()
  (interactive)
  (let ((form (completing-read  "Choose recently evaluated form: "
				(breeze-get-recently-evaluated-forms))))
    (when form
      (slime-interactive-eval form))))


;;; testing

(defun breeze-run-tests ()
  "Run tests."
  (interactive)
  (slime-repl-eval-string "(breeze.user:run-all-tests)"))


;;; integration to quicklisp

;; FIXME: this function is basically a copy-paste of breeze-quicklisp-local-project-directories
(defun breeze-quicklisp-list-systems ()
  "Get the list of quicklisp's local project directories."
  (car
   (read-from-string
    (cl-destructuring-bind (output value)
	(slime-eval `(swank:eval-and-grab-output
		      ,(format "%s" `(mapcar #'ql-dist:name (ql:system-list)))))
      value))))

;; (breeze-quicklisp-list-systems)

;; That works, but it hangs emacs while the system is loaded
(defun breeze-quickload ()
  (interactive)
  (let ((system (completing-read  "Choose a system to load: "
				(breeze-quicklisp-list-systems))))
    (when system
      (slime-eval `(ql:quickload ,system)))))


;;; project scaffolding

(defun breeze-quicklisp-local-project-directories ()
  "Get the list of quicklisp's local project directories."
  (car
   (read-from-string
    (cl-destructuring-bind (output value)
	(slime-eval `(swank:eval-and-grab-output
		      ,(format "%s" `(breeze.swank:get-ql-local-project-directories))))
      value))))

(defun breeze-choose-local-project-directories ()
  "Let the user choose the directory if there's more than one."
  (let ((directories (breeze-quicklisp-local-project-directories)))
    (if (eq 1 (length directories))
	(first directories)
      (completing-read "Choose a local-project directory: " directories))))

;; (breeze-choose-local-project-directories)

(defun breeze-quickproject ()
  "Create a project named NAME using quickproject."
  (interactive)
  (let ((name (read-string "Name of the project: "))
	;; TODO let the user choose a directory outside of quicklisp's local
	;; project directories.  see (read-directory-name "directory: ").
	(directory (breeze-choose-local-project-directories))
	;; TODO let the user choose
	(author breeze-default-author)
	(licence breeze-default-licence)
	;; TODO depends-on
	;; TODO include-copyright
	;; TODO template-directory
	;; TODO template-parameters
	)
    (slime-interactive-eval
     (concat
      "(breeze.swank:make-project \"" directory name "\""
      " :author \"" author "\""
      " :license \"" licence "\""
      ")"))
    (message "\"%s\" created" (concat directory name "/"))
    (find-file (concat directory name "/README.md"))))

;; e.g. (breeze-quickproject "foo")


;;; capture

(defun breeze-list-lisp-files (directory)
  ;; just the name, no extension, no directory
  (loop for file in
	(directory-files directory)
	when (and (not (string-prefix-p "." file))
		  (string-suffix-p ".lisp" file))
	collect (file-name-sans-extension file)))

;; TODO Use breeze-capture-template
(define-skeleton breeze-insert-header-template
  "" ;; TODO docstring
  "" ;; empty prompt. ignored.
  \n
  "(ql:quickload '(alexandria))" \n
  \n
  ";; make it easier to debug" \n
  "(declaim (optimize (speed 0) (safety 3) (debug 3)))" \n
  \n
  "#|" \n
  \n
  "goal:" \n
  \n
  "motivation:" \n
  \n
  "first lead:" \n
  \n
  "|#")

(defun breeze-capture ()
  ;; TODO docstring
  (interactive)
  ;; TODO check if directory exists, creates it if not.
  (let* ((name (completing-read
		"name of the file and package: "
		(breeze-list-lisp-files breeze-capture-folder)))
	 (file (concat breeze-capture-folder name ".lisp"))
	 (file-exists (file-exists-p file)))
    (find-file file)
    (unless file-exists
      (breeze-insert-defpackage))))


;;; managing threads

;; TODO
;; breeze-kill-worker-thread

;; (bordeaux-threads:destroy-thread
;;  (let ((current-thread (bt:current-thread)))
;;    (find-if #'(lambda (thread)
;; 		(and (not (eq current-thread thread))
;; 		     (string= "worker" (bt:thread-name thread))))
;; 	    (sb-thread:list-all-threads))))


;;; mode

(define-minor-mode breeze-mode
  "Breeze mimor mode."
  :lighter " brz"
  :keymap breeze-mode-map)

(define-key breeze-mode-map (kbd "C-c ,") 'breeze-insert)
(define-key breeze-mode-map (kbd "C-M-q") 'breeze-indent-defun-at-point)
(define-key breeze-mode-map (kbd "C-c c") 'breeze-capture)
(define-key breeze-mode-map (kbd "C-c t") 'breeze-run-tests)

;; eval keymap
(defvar breeze/eval-map (make-sparse-keymap))
(define-key breeze-mode-map (kbd "C-c e") breeze/eval-map)

(define-key breeze/eval-map (kbd "e") 'breeze-reevaluate-form)

;; TODO add defun "{en,dis}able-breeze-mode"

(provide 'breeze)
;;; breeze.el ends here
