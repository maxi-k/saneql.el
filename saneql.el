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
;;  Description
;;
;;; Code:

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

(define-derived-mode saneql-mode prog-mode "SaneQL"
  "Major mode for editing SaneQL files."
  (setq-local comment-start "--")
  (setq-local font-lock-defaults '((saneql-font-lock-keywords))))

(provide 'saneql)
;;; saneql.el ends here
