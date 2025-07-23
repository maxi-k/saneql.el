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
(require 'eieio)
(require 'cl-generic)

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

(defvar saneql-output-pop-to-buffer-p nil
  "Whether to pop to the output buffer after running a query.")

(defvar saneql-output-buffer-modes '(csv-mode csv-align-mode)
  "The modes to use for the output buffer.")

(defvar saneql-set-db-file-var-p nil
  "Whether to set the `saneql-db' file local variable when setting the database.")

(defvar saneql-db-file-hist nil
  "History of previously selected databse files for `savehist`.")

(defvar saneql-db-file-must-exist-p nil
  "Whether the database file must exist when setting the database interactively.")

;;; duckdb-specific options
(defvar saneql-duckdb-has-extension-p t
  "Whether the duckdb saneql extension is compiled into the duckdb binary.")

(defvar saneql-duckdb-default-binary "duckdb"
  "The default duckdb binary to use if none is specified.")

;;; sqlite-specific options
(defvar saneql-sqlite-default-binary "sqlite3"
  "The default sqlite binary to use if none is specified.")

;;; csv-specific options
(defvar saneql-csv-default-binary "duckdb"
  "The default database binary to use for csv files. Note: currently uses duckdb-specific csv syntax internally.")

(defvar saneql-here-db-map
  '((csv-mode . csv))
  "Alist mapping major modes to database types for `saneql-here'.")

(defvar saneql-here-extension-map
  '(("duckdb" . duckdb))
  "Alist mapping file extensions to database types for `saneql-here'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing a database connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass saneql-db-connection () ())
(defclass saneql-sqlite-connection (saneql-db-connection)
  ((filename :initarg :filename)
   (binary :initarg :binary)))

(defclass saneql-duckdb-connection (saneql-db-connection)
  ((filename :initarg :filename)
   (binary :initarg :binary)))

(defclass saneql-csv-connection (saneql-db-connection)
  ((filename :initarg :filename)
   (binary :initarg :binary)))

(setq saneql-db-type-alist
  `((sqlite . saneql-sqlite-connection)
    (csv . saneql-csv-connection)
    (duckdb . saneql-duckdb-connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill empty slots with default values for a database connection instance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Not using the :initform slot option because we want to be able to override
;; the default values in the user's init file.

(cl-defgeneric saneql--fill-defaults (db)
  "Fill in default values for the given database connection instance.")

(cl-defmethod saneql--fill-defaults ((db saneql-sqlite-connection))
  "Fill in default values for the given sqlite database connection instance.
Return the db instance."
  (unless (and (slot-boundp db :binary) (oref db :binary))
    (oset db :binary saneql-sqlite-default-binary))
  db)

(cl-defmethod saneql--fill-defaults ((db saneql-duckdb-connection))
  "Fill in default values for the given duckdb database connection instance.
Return the db instance."
  (unless (and (slot-boundp db :binary) (oref db :binary))
    (oset db :binary saneql-duckdb-default-binary))
  db)

(cl-defmethod saneql--fill-defaults ((db saneql-csv-connection))
  "Fill in default values for the given csv database connection instance.
Return the db instance."
  (unless (and (slot-boundp db :binary) (oref db :binary))
    (oset db :binary saneql-csv-default-binary))
  db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactively create a database connection instance, querying for user input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defgeneric saneql--make-connection (class &rest args)
  "Create a new database connection instance of the given class.")

(cl-defmethod saneql--make-connection ((class (subclass saneql-db-connection)) &rest args)
  "Default implementation for `saneql--make-connection' that throws an error."
  (user-error "connection type for %s not defined!" class))

(cl-defmethod saneql--make-connection ((class (subclass saneql-sqlite-connection)) &rest args)
  "Create a new sqlite connection instance."
  (let ((filename (read-file-name "db file: " nil saneql-db-file-hist saneql-db-file-must-exist-p)))
    (add-to-list 'saneql-db-file-hist filename)
    (if prefix-arg
        (saneql-sqlite-connection :filename filename
                                  :binary (completing-read "sqlite binary: " '()))
      (saneql--fill-defaults (saneql-sqlite-connection :filename filename)))))

(cl-defmethod saneql--make-connection ((class (subclass saneql-duckdb-connection)) &rest args)
  "Create a new duckdb connection instance."
  (let ((filename (read-file-name "db file: " nil saneql-db-file-hist saneql-db-file-must-exist-p)))
    (add-to-list 'saneql-db-file-hist filename)
    (if prefix-arg
        (saneql-duckdb-connection :filename filename
                                  :binary (completing-read "duckdb binary: " '()))
      (saneql--fill-defaults (saneql-duckdb-connection :filename filename)))))

(cl-defmethod saneql--make-connection ((class (subclass saneql-csv-connection)) &rest args)
  "Create a new duckdb connection instance."
  (let ((filename (read-file-name "db file: " nil saneql-db-file-hist saneql-db-file-must-exist-p)))
    (add-to-list 'saneql-db-file-hist filename)
    (if prefix-arg
        (saneql-csv-connection :filename filename
                               :binary (completing-read "database binary: " '()))
      (saneql--fill-defaults (saneql-csv-connection :filename filename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saneql-set-db (db-type &optional connection-args)
  "Interactively set the database connection to use for evaluating SaneQL expressions."
  (interactive (list (completing-read "connection type: "
                                      (mapcar #'car saneql-db-type-alist))))
  (let* ((class (alist-get (intern db-type) saneql-db-type-alist))
         (db-instance (saneql--make-connection class)))
    (setq-local saneql-db db-instance)
    (when saneql-set-db-file-var-p
      (add-file-local-variable-prop-line 'saneql-db db-instance))
    db-instance))

(defun saneql--set-db-directly (db-type &optional connection-args)
  "Set the database connection to use for evaluating SaneQL expressions.
Do not use --make-connection functions (which query for user input), but construct
the class directly."
  (let* ((class (alist-get (if (symbolp db-type) db-type (intern db-type)) saneql-db-type-alist))
         (db-instance (saneql--fill-defaults (apply #'make-instance class connection-args))))
    (setq-local saneql-db db-instance)
    (when saneql-set-db-file-var-p
      (add-file-local-variable-prop-line 'saneql-db db-instance))
    db-instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database-specific settings 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defgeneric saneql--db-native-saneql-support (db)
  "Return whether the given database supports native SaneQL evaluation.")

(cl-defmethod saneql--db-native-saneql-support ((db saneql-sqlite-connection))
  "Sqlite does not support native SaneQL evaluation."
  nil)

(cl-defmethod saneql--db-native-saneql-support ((db saneql-duckdb-connection))
  "DuckDB supports native SaneQL evaluation if the extension is installed."
  saneql-duckdb-has-extension-p)

(cl-defmethod saneql--db-native-saneql-support ((db saneql-csv-connection))
  "DuckDB supports native SaneQL evaluation if the extension is installed."
  (and saneql-duckdb-has-extension-p
       (s-contains-p "duckdb" (oref db :binary))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query a database 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defgeneric saneql--query-database (db output-buffer)
  "Assuming `current-buffer' contains a sql query, use the given
db connection instance to query the database and write the result to the output buffer.")

(cl-defmethod saneql--query-database ((db saneql-sqlite-connection) output-buffer)
  "Execute the current buffer as sql against the given sqlite database and write the result to the output buffer."
  (with-slots (filename binary) db
    (call-process-region (point-min) (point-max) (or  binary saneql-sqlite-default-binary)
                         ;; TODO make less sqlite specific
                         nil output-buffer nil "-cmd" ".headers on" "-cmd" ".separator ," "-cmd" ".read '|cat -'"
                         (if (or (not filename) (eq filename "") (eq filename ":memory:")) nil (expand-file-name (format "%s" filename))))))

(cl-defmethod saneql--query-database ((db saneql-duckdb-connection) output-buffer)
  "Execute the current buffer as sql against the given duckdb database and write the result to the output buffer."
  (with-slots (filename binary) db
    (call-process-region (point-min) (point-max) (or binary saneql-duckdb-default-binary)
                         nil output-buffer nil "-header" "-csv" "-cmd" ".separator ,"
                         (if (or (not filename) (eq filename "") (eq filename ":memory:")) nil (expand-file-name (format "%s" filename))))))

(cl-defmethod saneql--query-database ((db saneql-csv-connection) output-buffer)
  "Execute the current buffer as sql against the given csv file and write the result to the output buffer. The csv table is called 'db'"
  (with-slots (filename binary) db
    (let ((csv-read-str
           (let ((text-quoting-style 'straight))
             (format "create table db as (select * from read_csv_auto('%s'))"
                     (expand-file-name (format "%s" filename))))))
      ;; (message csv-read-str)
      (call-process-region (point-min) (point-max) (or binary saneql-csv-default-binary)
                           nil output-buffer nil
                           "-header" "-csv" "-cmd" ".separator ,"
                           "-cmd" csv-read-str))))

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

(defun saneql--copy-buffer (up-to-point)
  "Copy the current buffer up to the given point to a compilation buffer and return it."
  (let ((inhibit-message t)
        (saved-point (point))
        (start (point-min))
        (end (point-max))
        (buf (current-buffer)))
    (when up-to-point
      (move-end-of-line nil)
      (setq end (point))
      (goto-char saved-point))
    (let ((compilation-buf (get-buffer-create saneql-compilation-buffer-name)))
      (with-current-buffer compilation-buf
        (erase-buffer)
        (insert-buffer-substring buf start end))
      compilation-buf)))

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
         (result-buf (if (saneql--db-native-saneql-support db)
                         (saneql--exec-sql-buffer db (saneql--copy-buffer up-to-point))
                       (saneql--exec-sql-buffer db (saneql--compile-buffer up-to-point)))))
    (with-current-buffer result-buf
      (mapc #'funcall saneql-output-buffer-modes)
      (goto-char (point-min)))
    (unless (get-buffer-window result-buf)
      (display-buffer result-buf))
    (when saneql-output-pop-to-buffer-p
      (pop-to-buffer result-buf))))

(defun saneql-here ()
  "Open a saneql buffer for the current buffer.
If the current buffer is not in a supported mode (see `saneql-here-mode-map'),
query for a file."
  (interactive)
  (let ((db-type (alist-get major-mode saneql-here-db-map))
        (source-file (buffer-file-name))
        (short-name (buffer-name)))
    (unless db-type  ;; current buffer not in a supported mode
      (setq source-file (read-file-name "input file: " nil saneql-db-file-hist saneql-db-file-must-exist-p))
      (if-let* ((extension (file-name-extension source-file))
                (found-db-type (alist-get extension saneql-here-extension-map nil nil #'string-equal)))
          (progn
            (setq db-type found-db-type)
            (setq short-name (file-name-base source-file)))
        (save-excursion
          (find-file source-file)
          (setq db-type (alist-get major-mode saneql-here-db-map))
          (setq short-name (buffer-name))
          (unless db-type
            (user-error "no database type for mode %s" major-mode)))))
    ;; create new *saneql* buffer and set saneql-db
    (let ((buf (get-buffer-create (format "*saneql: %s*" short-name))))
      (with-current-buffer buf
        (saneql-mode)
        (saneql--set-db-directly db-type (list :filename source-file))
        (goto-char (point-min)))
      (pop-to-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar saneql-font-lock-keywords
  (let* (
         (keywords '("let" "defun" "null" "true" "false" "table" "&&" "||"))
         (table-fns '("filter" "join" "groupby" "aggregate" "distinct" "orderby" "map" "project" "projectout" "as" "alias" "union" "except" "intersect" "window"))
         (scalar-fns '("asc" "desc" "collate" "is" "between" "in" "like" "substr" "extract"))
         (free-fns '("count" "sum" "avg" "min" "max"
                     "row_number" "lead" "lag" "first_row" "last_row" "rank" "dense_rank" "ntile"
                     "table" "case" "gensym" "foreigncall"))
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
         (constants-regexp (regexp-opt constants 'words)))
    `((,keywords-regexp . 'font-lock-keyword-face)
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
    ;; strings with single quotes
    (modify-syntax-entry ?' "\"" st)
    ;; make sure we don't treat _ as a word separator so that for example
    ;; 'c_count' does not highlight 'count' as a keyword
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `saneql-mode'.
Automatically discovered by `define-derived-mode' due to its name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Definition and Keymap 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar saneql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'saneql-compile-and-run-buffer)
    (define-key map (kbd "C-RET") #'saneql-compile-and-run-buffer)
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
  (add-to-list 'auto-mode-alist '("\\.saneql\\'" . saneql-mode)))

(provide 'saneql)
;;; saneql.el ends here
