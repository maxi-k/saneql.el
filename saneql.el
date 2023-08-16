;;; saneql.el --- Major Mode for SaneQL -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Maximilian Kuschewski
;;
;; Author: Maximilian Kuschewski <>
;; Maintainer: Maximilian Kuschewski <>
;; Created: August 16, 2023
;; Modified: August 16, 2023
;; Version: 0.0.1
;; Keywords: data languages database
;; Homepage: https://github.com/maxi-k/saneql
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description:
;;
;; Major mode for editing SaneQL files. See github.com/neumannt/saneql for more information on the language.
;;
;; Mode Features:
;;
;; - C-c C-c compiles the buffer, runs the given sql against the database and displays the output in a new buffer
;; - C-u C-c C-c compiles and runs the buffer up to the current line
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exposed options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local saneql-db nil
  "The database connection to use for evaluating SaneQL expressions.
Use `saneql-set-db' to set this variable")

(defvar-local saneql-compile-command "saneql"
  "The command to use for compiling SaneQL expressions to SQL.")

(defvar-local saneql-tempfile "/tmp/buffer.sane"
  "The temporary file to use for compiling SaneQL expressions.")

(defvar saneql-compilation-buffer-name "*saneql-compilation*"
  "The name of the buffer to use for storing compiled SQL.")

(defvar saneql-output-buffer-name "*saneql-output*"
  "The name of the buffer to use for displaying SaneQL output.")

(defvar saneql-output-buffer-modes '(csv-mode csv-align-mode)
  "The modes to use for the output buffer.")

(defvar saneql-set-db-file-var-p t
  "Whether to set the `saneql-db' file local variable when setting the database.")

(defvar saneql-db-file-hist nil
  "History of previously selected databse files for `savehist`.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing a database connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass saneql-db-connection () ())
(defclass saneql-sqlite-connection (saneql-db-connection)
  ((filename :initarg :filename)
   (binary :initarg :binary :initform "sqlite3")))

(defclass saneql-csv-connection (saneql-db-connection)
  ((filename :initarg :filename)
   (binary :initarg :binary :initform "sqlite3")))

(setq saneql-db-type-alist
  `((sqlite . saneql-sqlite-connection)
    (csv . saneql-csv-connection)))

(defgeneric saneql--make-connection (class &rest args))

(defmethod saneql--make-connection ((class (subclass saneql-db-connection)) &rest args)
  (user-error "connection type for %s not defined!" class))

(defmethod saneql--make-connection ((class (subclass saneql-sqlite-connection)) &rest args)
  (let ((filename (read-file-name "db file: " nil saneql-db-file-hist t)))
    (add-to-list 'saneql-db-file-hist filename)
    (if prefix-arg
        (saneql-sqlite-connection :filename filename
                                  :binary (completing-read "sqlite binary: " '()))
        (saneql-sqlite-connection :filename filename))))

(defun saneql-set-db (db-type &optional connection-args)
  (interactive (list (completing-read "connection type: "
                                      (mapcar #'car saneql-db-type-alist))))
  (let* ((class (alist-get (intern db-type) saneql-db-type-alist))
         (db-instance (saneql--make-connection class)))
    (setq-local saneql-db db-instance)
    (when saneql-set-db-file-var-p
      (add-file-local-variable-prop-line 'saneql-db db-instance))
    db-instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query a database 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric saneql--query-database (db output-buffer)
  "Assuming `current-buffer' contains a sql query, use the given
db connection instance to query the database and write the result to the output buffer.")

(defmethod saneql--query-database ((db saneql-sqlite-connection) output-buffer)
  (with-slots (filename binary) db
    (call-process-region (point-min) (point-max) binary
                         ;; TODO make less sqlite specific
                         nil output-buffer nil "-cmd" ".headers on" "-cmd" ".separator ," "-cmd" ".read '|cat -'"
                         (expand-file-name (format "%s" filename)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compling and running a buffer 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saneql--compile-buffer (up-to-point)
  "Compile the current buffer to SQL and return the compilation
buffer where the output sql is stored.
If UP-TO-POINT is non-nil, only compile up to the current line."
  (let ((inhibit-message t)
        (saved-point (point))
        (end (point-max)))
    (when up-to-point
      (move-end-of-line nil)
      (setq end (point))
      (goto-char saved-point))
    (write-region (point-min) end saneql-tempfile nil nil nil nil))
  (let ((compilation-buf (get-buffer-create saneql-compilation-buffer-name)))
    (with-current-buffer compilation-buf
      (erase-buffer)
      (call-process saneql-compile-command nil t nil saneql-tempfile))
    compilation-buf))

(defun saneql--exec-sql-buffer (db buffer)
  "Execute the given sql buffer against the given database and return the result buffer."
  (let ((output-buf (get-buffer-create saneql-output-buffer-name)))
    (with-current-buffer output-buf (erase-buffer))
    (with-current-buffer buffer
      (saneql--query-database
       db
       output-buf)
    output-buf)))

(defun saneql-compile-and-run-buffer (up-to-point)
  "Compile the current buffer to SQL and run the resulting SQL against the database.
If UP-TO-POINT is non-nil, only compile up to the current line.
If no database connection is set, prompt for one. Switches to the
output buffer afterwards."
  (interactive "P")
  (let* ((db (or saneql-db (call-interactively #'saneql-set-db)) )
         (result-buf (saneql--exec-sql-buffer db (saneql--compile-buffer up-to-point))))
    (with-current-buffer result-buf
      (mapc #'funcall saneql-output-buffer-modes)
      (goto-char (point-min)))
    (pop-to-buffer result-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar saneql-font-lock-keywords
  (let* (
         (keywords '("let" "defun" "null" "true" "false" "table" "&&" "||"))
         (table-fns '("filter" "join" "groupby" "aggregate" "distinct" "orderby" "map" "project" "projectout" "as" "alias" "union" "except" "intersect" "window"))
         (scalar-fns '("asc" "desc" "collate" "is" "between" "in" "like" "substr" "extract"))
         (free-fns '("count" "sum" "avg" "min" "max" "row_number" "table" "case" "gensym" "foreigncall"))
         (types '("integer" "boolean" "date" "interval" "text"))
         (constants '("function" "leftassoc" "rightassoc" "operator"
                      "inner" "left" "leftouter" "right" "rightouter" "full"
                      "fullouter" "leftsemi" "exists" "rightsemi" "leftanti"
                      "notexists" "rightanti"))
         (keywords-regexp (regexp-opt keywords 'words))
         (table-fns-regexp (regexp-opt table-fns 'words))
         (scalar-fns-regexp (regexp-opt scalar-fns 'words))
         (free-fns-regexp (regexp-opt free-fns 'words))
         (types-regexp (regexp-opt types 'words))
         (constants-regexp (regexp-opt constants 'words))
         (string-regexp "'\\([^']*\\)'"))
    `((,string-regexp . 'font-lock-string-face)
      (,keywords-regexp . 'font-lock-keyword-face)
      (,types-regexp . 'font-lock-type-face)
      (,constants-regexp . 'font-lock-constant-face)
      (,table-fns-regexp . 'font-lock-builtin-face)
      (,scalar-fns-regexp . 'font-lock-function-name-face)
      (,free-fns-regexp . 'font-lock-function-name-face)))
  "Keyword highlighting specification for `saneql-mode'.")

(defvar saneql-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?- ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `saneql-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Definition and Keymap 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar saneql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'saneql-compile-and-run-buffer)
    map)
  "Keymap for `saneql-mode'.")

;;;###autoload
(define-derived-mode saneql-mode prog-mode "SaneQL"
  "Major mode for editing SaneQL files."
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '((saneql-font-lock-keywords)))
  (use-local-map saneql-mode-map)

  (if (bound-and-true-p savehist-loaded)
      (add-to-list 'savehist-additional-variables 'saneql--database-file-hist)
    (add-hook 'savehist-mode-hook
              (lambda ()
                (add-to-list 'savehist-additional-variables 'saneql--database-file-hist)))))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.sane\\'" . saneql-mode))
  (add-to-list 'auto-mode-alist '("\\.saneql\\'" . saneql-mode))


  )

(provide 'saneql)
;;; saneql.el ends here
