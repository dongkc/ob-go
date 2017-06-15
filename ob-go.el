
;;; ob-go.el --- org-babel functions for go evaluation

;; Copyright (C) 2012 K. Adam Christensen

;; Author: K. Adam Christensen
;; Keywords: golang, go, literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.02

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating go code.
;;
;; Much of this is modeled after `ob-C'. Just like the `ob-C', you can specify
;; :flags headers when compiling with the "go run" command. Unlike `ob-C', you
;; can also specify :args which can be a list of arguments to pass to the
;; binary. If you quote the value passed into the list, it will use `ob-ref'
;; to find the reference data.
;;
;; If you do not include a main function or a package name, `ob-go' will
;; provide it for you and it's the only way to properly use
;;
;; very limited implementation:
;; - currently only support :results output.
;; - not much in the way of error feedback.
;; - cannot handle table or list input.

;;; Requirements:

;; - You must have go1 installed and the go should be in your `exec-path'. If
;;   not, feel free to modify `org-babel-go-command' to the location of your
;;   go command.
;;
;; - `go-mode' is also recommended for syntax highlighting and
;;   formatting. Not this particularly needs it, it just assumes you
;;   have it.

;;; TODO:

;; - Provide better error feedback.
;;
;; - Figure out a nice way to handle lists and tables.
;;
;; - Do some simple parsing of the go code to insert vars right after the
;;   package declaration.

;;; Code:
(require 'org)
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)


;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("go" . "go"))

(defvar org-babel-default-header-args:go '())

(defvar org-babel-go-command "go"
  "The go command to use to compile and run the go code.")

(defmacro org-babel-go-as-list (val)
  (list 'if (list 'listp val) val (list 'list val)))

(defun org-babel-expand-body:go (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel-go-get-var params))
         (colnames (cdr (assq :colname-names params)))
         (main-p (not (string= (cdr (assoc :main params)) "no")))
         (imports (or (cdr (assoc :imports params))
                      (org-babel-read (org-entry-get nil "imports" t))))
         (package (or (cdr (assoc :package params))
                      (org-babel-read (org-entry-get nil "package" t))))
         (body (if main-p (org-babel-go-ensure-main-wrap body) body))
         )
    (org-babel-go-custom-colnames
     (org-babel-go-custom-vars
      (org-babel-go-custom-imports
       (org-babel-go-ensure-package body package)
       imports)
      vars)
     colnames)))

(defun org-babel-execute:go (body params)
  "Execute a block of Template code with org-babel. This function is
called by `org-babel-execute-src-block'"
  (message "executing Go source code block")
  (let* ((tmp-src-file (org-babel-temp-file "go-src-" ".go"))
         (processed-params (org-babel-process-params params))
         (flags (cdr (assoc :flags processed-params)))
         (args (cdr (assoc :args processed-params)))
         ;; expand the body with `org-babel-expand-body:go'
         (full-body (org-babel-expand-body:go
                     body params processed-params))
         (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
         (coding-system-for-write 'utf-8))
    (with-temp-file tmp-src-file (insert full-body))
    (let ((results
	      (org-babel-eval
	       (format "%s run %s \"%s\" %s"
		       org-babel-go-command
		       (mapconcat 'identity (org-babel-go-as-list flags) " ")
		       (org-babel-process-file-name tmp-src-file)
		       (mapconcat #'(lambda (a)
				      ;; If there's a chance that the symbol is a
				      ;; ref, use that. Otherwise, just return the
				      ;; string form of the value.
				      (format "%S" (if (symbolp a)
						       (let* ((ref (symbol-name a))
							      (out (org-babel-read ref)))
							 (if (equal out ref)
							     (if (string-match "^\".*\"$" ref)
								 (read ref)
							       (org-babel-ref-resolve ref))
							   out))
						     a)))
				  (org-babel-go-as-list args) " ")) "")))
	(org-babel-reassemble-table
	 (if (or (member "table" (cdr (assoc :result-params processed-params)))
		 (member "vector" (cdr (assoc :result-params processed-params))))
	     (let ((tmp-file (org-babel-temp-file "go-")))
	       (with-temp-file tmp-file (insert (org-babel-trim results)))
	       (org-babel-import-elisp-from-file tmp-file))
	   (org-babel-read (org-babel-trim results) t))
	 (org-babel-pick-name
	  (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
	 (org-babel-pick-name
	  (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:go (session params)
  "This function does nothing as Go is a compiled language with no
support for sessions"
  (error "Go is a compiled languages -- no support for sessions"))

(defun org-babel-go-ensure-main-wrap (body)
  "Check to see if main is already defined. If not, add it."
  (if (string-match-p "^[ \t]*func main *() *{" body)
      body
    (concat "func main() {\n" body "\n}\n")))

(defun org-babel-go-append-package (package)
  "Check to see if package is set. If not, add main."
  (concat "package " (if (and package (not (string-empty-p package))) package "main")))

(defun org-babel-go-ensure-package (body package)
  "Ensure package exists."
  (if (org-babel-go-package-p body)
      body
    (concat (org-babel-go-append-package package) "\n" body)))

(defun org-babel-go-package-p (body)
  "Check to see whether package is set or not."
  (string-match-p "^[ \t]*package " body))

(defun org-babel-go-package-position (body)
  (string-match "^[ \t]*package " body))

(defun org-babel-go-custom-imports (body imports)
  "Append custom import packages."
  (let* ((start (string-match "\n"
                              (substring body
                                         (org-babel-go-package-position body)))))
    (concat (substring body 0 start)
            "\n"
            (org-babel-go-insert-imports imports)
            (substring body start))))

(defun org-babel-go-insert-imports (imports)
  (let ((packages (org-babel-go-as-list imports)))
    (if (= (length packages) 0)
        ""
      (concat "import ("
              "\n\t"
              (mapconcat #'(lambda (pkg) (format "%S" pkg))
                         packages
                         "\t\n")
              "\n)"
              "\n"))))

(defun org-babel-go-custom-vars (body vars)
  "Append custom variables at bottom."
  (if (=  (length vars) 0)
      body
    (concat body "\n" (mapconcat 'org-babel-go-var-to-go vars "\n") "\n")))

(defun org-babel-go-custom-colnames (body colnames)
  (if colnames
    (concat
     body
     "\n"
     (concat (org-babel-go-utility-header-to-go) "\n\n")
     (mapconcat 'org-babel-go-header-to-go colnames "\n"))
    body)
  )

(defun org-babel-go-get-var (params)
  "org-babel-get-header was removed in org version 8.3.3"
  (let* ((fversion (org-version))
         (version (string-to-int fversion)))
    (if (< version 8.3)
        (mapcar #'cdr (org-babel-get-header params :var))
      (org-babel--get-vars params))))

(defun org-babel-go-gofmt (body)
  "Run gofmt over the body. Why not?"
  (with-temp-buffer
    (let ((outbuf (current-buffer))
          (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
          (coding-system-for-write 'utf-8))
      (with-temp-buffer
        (insert body)
        (shell-command-on-region (point-min) (point-max) "gofmt"
                                 outbuf nil nil)))
    (buffer-string)))

(defun org-babel-go-var-to-go (pair)
  "Convert an elisp val into a string of go code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (let* ((type-data (org-babel-go-val-to-go-type val))
           (type (car type-data))
           (formated (org-babel-go-format-val type-data val))
           (prefix (car formated))
           (data (cdr formated)))
      (format "var %s %s = %s"
              var
              (concat prefix type)
              data))))

(defun org-babel-go-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
        (cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-go-val-to-base-type (val)
  "Determine the base type of VAL which may be
`integerp' if all base values are integers
`floatp' if all base values are either floating points or integers
`stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ((and (listp val) (listp (car val))) 'stringp)
   ((or (listp val) (vectorp val))
    (let ((type nil))
      (mapc (lambda (v)
              (pcase (org-babel-go-val-to-base-type v)
                (`stringp (setq type 'stringp))
                (`floatp
                 (unless (setq type 'floatp)))
                (`integerp
                 (unless type (setq type 'integerp)))))
            val)
      type))
   (t 'stringp)))

(defun org-babel-go-val-to-go-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL."
  (let* ((basetype (org-babel-go-val-to-base-type val))
         (type
          (pcase basetype
            (`integerp '("int" "%d"))
            (`floatp '("float32" "%f"))
            (`stringp '("string" "\"%s\""))
            (_ (error "unknown type %S" basetype)))))
    (cond
     ((integerp val) type) ;; an integer declared in the #+begin_src line
     ((floatp val) type) ;; a numeric declared in the #+begin_src line
     ((and (listp val) (listp (car val))) ;; a table
      `(,(car type)
        (lambda (val)
          (cons "[][]"
                (concat "[][]string{\n"
                        (mapconcat
                         (lambda (v)
                           (concat
                            " []string{"
                            (mapconcat (lambda (w) (format ,(cadr type) w)) v ",")
                            "},"))
                         val
                         "\n")
                        "\n}")))))
     ((or (listp val) (vectorp val)) ;; a list declared in the #+begin_src line
      `(,(car type)
        (lambda (val)
          (cons "[]"
                (concat "[]" ,(car type) "{"
                        (mapconcat (lambda (v) (format ,(cadr type) v)) val ",")
                        "}")))))
     (t ;; treat unknown types as string
      type))))

(defun org-babel-go-utility-header-to-go ()
  "Generate a utility function to convert a column name into a column number."
  "func get_column_num(header []string, column string) int {
  for index, _ := range header {
    if header[index] == column {
       return index
    }
  }
  return -1
}")

(defun org-babel-go-header-to-go (head)
  "Convert an elisp list of header table into a go vector
specifying a variable with the name of the table."
  (let ((table (car head))
        (headers (cdr head)))
    (concat
     (format
      "var %s_header []string = []string{%s}"
      table
      (mapconcat (lambda (h) (format "%S" h)) headers ","))
     "\n"
     (format
      "func %s_helper(row int, col string) string {
  return %s[row][get_column_num(%s_header,col)]
}"
      table table table))))

(provide 'ob-go)
;;; ob-go.el ends here
