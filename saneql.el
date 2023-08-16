;;; saneql.el --- Major Mode for SaneQL -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Maximilian Kuschewski
;;
;; Author: Maximilian Kuschewski <>
;; Maintainer: Maximilian Kuschewski <>
;; Created: August 16, 2023
;; Modified: August 16, 2023
;; Version: 0.0.1
;; Keywords: data languages
;; Homepage: https://github.com/maxi/saneql
;; Package-Requires: ((emacs "24.3"))
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
;;  Example Buffer:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; saneql-db: ~/dev/tpch-sqlite/tpch.db -*-
;; -- the above line sets the input database for evaluation
;; let base := '1993-10-01',
;; let basedate(add := '+0 seconds') := foreigncall('date', date, {base, add}),
;; orders
;; .filter(o_orderdate >= basedate() && o_orderdate < basedate(add := '+3 months'))
;; .join(customer, c_custkey=o_custkey)
;; .join(lineitem.filter(l_returnflag='R'), l_orderkey=o_orderkey)
;; .join(nation, c_nationkey=n_nationkey)
;; .groupby({c_custkey, c_name, c_acctbal, c_phone, n_name, c_address, c_comment}, {revenue:=sum(l_extendedprice * (1 - l_discount))})
;; .orderby({revenue.desc()}, limit:=20)
;; .project({c_custkey, c_name, revenue})
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

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
;; Commands & Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local saneql-db nil
  "The database connection to use for evaluating SaneQL expressions.")

(defvar-local saneql-compile-command "saneql"
  "The command to use for compiling SaneQL expressions to SQL.")

(defvar-local saneql-sql-exec-command "sqlite3"
  "The command to use for evaluating SQL. The command should accept
a database connection string as the first argument and the SQL
query as the second argument.")

(defvar-local saneql-tempfile "/tmp/buffer.sane"
  "The temporary file to use for compiling SaneQL expressions.")

(defvar saneql-compilation-buffer-name "*saneql-compilation*"
  "The name of the buffer to use for storing compiled SQL.")

(defvar saneql-output-buffer-name "*saneql-output*"
  "The name of the buffer to use for displaying SaneQL output.")

(defvar saneql-output-buffer-modes '(csv-mode csv-align-mode)
  "The modes to use for the output buffer.")

(defun saneql--compile-buffer (up-to-point)
  (let ((inhibit-message t)
        (saved-point (point))
        (end (point-max)))
    (when up-to-point
      (move-end-of-line nil)
      (setq end (point))
      (goto-char saved-point))
    (write-region (point-min) end saneql-tempfile nil nil nil nil))
  (let ((compilation-buf (get-buffer-create saneql-compilation-buffer-name t)))
    (with-current-buffer compilation-buf
      (erase-buffer)
      (call-process saneql-compile-command nil t nil saneql-tempfile))
    compilation-buf))

(defun saneql--exec-sql-buffer (db buffer)
  (let ((output-buf (get-buffer-create saneql-output-buffer-name)))
    (with-current-buffer output-buf (erase-buffer))
    (with-current-buffer buffer
    (message "db %s buffer %s out %s" db buffer output-buf)
      (call-process-region (point-min) (point-max) saneql-sql-exec-command
                           ;; TODO make less sqlite specific
                           nil output-buf nil "-cmd" ".headers on" "-cmd" ".separator ," "-cmd" ".read '|cat -'"
                           (expand-file-name (format "%s" db))))
    output-buf))

(defun saneql-compile-and-run-buffer (up-to-point)
  (interactive "P")
  (let* ((db saneql-db)
        (result-buf (saneql--exec-sql-buffer db (saneql--compile-buffer up-to-point))))
    (with-current-buffer result-buf
        (mapc #'funcall saneql-output-buffer-modes)
        (goto-char (point-min)))
    (pop-to-buffer result-buf)))

(defvar saneql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'saneql-compile-and-run-buffer)
    map)
  "Keymap for `saneql-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode saneql-mode prog-mode "SaneQL"
  "Major mode for editing SaneQL files."
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '((saneql-font-lock-keywords)))
  (use-local-map saneql-mode-map))

(provide 'saneql)
;;; saneql.el ends here
