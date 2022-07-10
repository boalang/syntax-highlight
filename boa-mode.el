;;; boa-mode.el --- Mode for boa language files

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.4.3
;; Package-Requires: ((cc-mode "5.33.1"))
;; Keywords: boa, msr, language
;; URL: https://github.com/boalang/syntax-highlight


;; Copyright 2022 Samuel W. Flint and University of Nebraska Board of Regents
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;

;; This package provides basic language support for Boa
;; (https://boa.cs.iastate.edu).  More features are coming.

(require 'cc-langs)
(require 'cc-mode)
(require 'cl-lib)

;;; Code:


;;; Basic Syntax

(defvar boa-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?# "<. " table)
    (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?@ "$" table)
    table)
  "Syntax table for `boa-mode'.")

(defvar boa-errors '("goto" "const")
  "List of error keywords in `boa-mode'")

(defvar boa-keywords '("if" "else" "switch" "before" "after" "case" "default"
                       "while" "for" "do" "foreach" "exists" "ifall" "visit" "function"
                       "return" "stop" "visit" "break" "continue" "skipwhite")
  "List of keywords for the Boa language.")

(defvar boa-constants '("true" "false" "input")
  "List of known constants for Boa.")

(defvar boa-types '("bool" "byte" "int" "float" "string" "time" "fingerprint" "visitor"
                    "RepositoryKind" "FileKind" "ChangeKind" "StatementKind" "ExpressionKind" "Visibility"
                    "ModifierKind" "CommentKind"
                    "of" "stack" "map" "output" "array" "input" "weight"
                    "Project" "CodeRepository" "Person" "Revision" "ChangedFile" "ASTRoot"
                    "Namespace" "Declaration" "Type" "Method" "Variable" "Statement" "Expression" "Modifier"
                    "sum" "collection" "top" "bottom" "set" "minimum" "maximum" "mean")
  "List of known types for the Boa language.")

(defvar boa-builtins '("acosh" "addday" "addmonth" "addweek" "addyear" "asinh"
                       "assert" "assignlatestvalue" "atanh" "cnf" "collect_annotations"
                       "collect_generic_types" "converttoarray" "converttosymbolicname"
                       "dayofmonth" "dayofweek" "dayofyear" "debug" "def" "dnf" "dot"
                       "format" "formattime" "get_annotation" "getast" "getastcount"
                       "getcdg" "getcfg" "getcfgslice" "getcomments" "getcrypthash"
                       "getddg" "getissues" "getlang" "get_metric_noa" "get_metric_noc"
                       "get_metric_noo" "get_metric_npm" "getnoargsvariables"
                       "get_nodes_with_definition" "getpdg" "getpdgslice" "getpdtree"
                       "getpreviousversion" "getrevision" "getrevisionindex"
                       "getrevisionindex2" "getrevisionscount" "getsnapshot"
                       "getsnapshotbyid" "getsnapshotbyindex" "get_variable_def"
                       "get_variable_killed" "get_variable_used" "has_annotation"
                       "hasfiletype" "has_modifier" "has_modifier_final" "has_modifier_namespace"
                       "has_modifier_private" "has_modifier_protected" "has_modifier_public"
                       "has_modifier_static" "has_modifier_synchronized" "has_visibility"
                       "hourof" "isboollit" "ischarlit" "isfinite" "isfixingrevision"
                       "isfloatlit" "isintlit" "iskind" "isliteral" "isnan" "isnormal"
                       "isnulllit" "isstringlit" "istypelit" "join" "lowercase" "match"
                       "matchposns" "matchstrs" "minuteof" "monthof" "new" "nnf" "normalize"
                       "now" "nrand" "parse" "parseexpression" "prettyprint" "push"
                       "pop" "rand" "reduce" "secondof" "setlang" "simplify" "sort"
                       "split" "splitn" "strfind" "strreplace" "strrfind" "substring"
                       "trim" "trunc" "trunctoday" "trunctohour" "trunctominute"
                       "trunctomonth" "trunctosecond" "trunctoyear" "type_name"
                       "uppercase" "url" "yearof")
  "List of builtins for `boa-mode'.")

(defvar boa-mode-font-lock-keywords
  `((("\"\\.\\*\\?" . font-lock-string-face)
     (,(regexp-opt boa-errors 'symbols) . font-lock-warning-face)
     (,(regexp-opt boa-keywords 'symbols) . font-lock-keyword-face)
     (,(regexp-opt boa-constants 'symbols) . font-lock-constant-face)
     (,(regexp-opt boa-types 'symbols) . font-lock-type-face)
     (,(regexp-opt boa-builtins 'symbols) . font-lock-builtin-face)))
  "Boa font-locking configuration.")


;;; Abbreviations and Snippets
(define-abbrev-table 'boa-mode-abbrev-table
  '()
  "Abbrev table for Boa mode."
  :parents (list c-mode-abbrev-table))

;; Yasnippet loading code taken in part from yasnippet-radical-snippets
(defconst boa-mode-snippets-dir
  (expand-file-name
   "emacs-snippets"
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

(defun boa-mode-enable-snippets ()
  "Load snippets for Boa mode."
  (add-to-list 'yas-snippet-dirs boa-mode-snippets-dir t)
  (yas-load-directory boa-mode-snippets-dir t))

(with-eval-after-load 'yasnippet
  (boa-mode-enable-snippets))


;; Study Template Support
(defvar-local boa-project-dir nil
  "Directory of current Boa project.")

(defvar-local boa-file-relative-name nil
  "Relative name of Boa file.")

(defvar-local boa-project-study-config-p nil
  "Boa study configuration file presence, nil if not present, Lisp timestamp if present.")

(defun boa--study-config-file ()
  (expand-file-name "study-config.json" boa-project-dir))

(defvar-local boa-project-study-config nil
  "Boa project study configuration.")

(defvar-local boa-project-query-outputs nil
  "List of possible outputs for the current Boa query file.")

(defvar-local boa-project-csv-outputs nil
  "List of possible CSV outputs for the current Boa query file.")

(defvar-local boa-project-query-analyses nil
  "List of analyses for the current Boa query file.")

(defun boa--parse-study-config ()
  (when  (and (fboundp 'json-parse-buffer)
            (or (null boa-project-study-config)
               (< (time-convert boa-project-study-config-p 'integer)
                  (time-convert (file-attribute-modification-time (file-attributes (boa--study-config-file))) 'integer))))
    (message "Parsing Study Configuration.")
    (setq-local boa-project-study-config
                (let ((file (boa--study-config-file)))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer)))
                boa-project-study-config-p (file-attribute-modification-time (file-attributes (boa--study-config-file))))
    (let ((current-outputs (list))
          (current-csvs (list))
          (current-analyses (list)))
      (maphash #'(lambda (key value)
                   (when (string= boa-file-relative-name (gethash "query" value))
                     (push key current-outputs)
                     (when-let ((csv-decl (gethash "csv" value)))
                       (push (gethash "output" csv-decl) current-csvs))))
               (gethash "queries" boa-project-study-config))
      (maphash #'(lambda (key value)
                   (when (intersection (append current-outputs current-csvs)
                                       (mapcar #'identity (gethash "input" value))
                                       :test #'string=)
                     (push key current-analyses)))
               (gethash "analyses" boa-project-study-config))
      (setq-local boa-project-query-outputs (mapcar #'(lambda (output) (format "data/txt/%s" output)) current-outputs)
                  boa-project-csv-outputs (mapcar #'(lambda (csv) (format "data/csv/%s" csv)) current-csvs)
                  boa-project-query-analyses (mapcar #'file-name-sans-extension current-analyses)))))

(defvar-local boa-run-verbose nil
  "Should boa queries run with verbosity?")

(defun boa-compile (target)
  (let ((verboseness (cond
                      ((integerp boa-run-verbose)
                       (format " VERBOSE=-%s" (make-string boa-run-verbose ?v)))
                      (boa-run-verbose
                       " VERBOSE=-v")
                      (t ""))))
    (let  ((compilation-directory boa-project-dir)
           (default-directory boa-project-dir))
      (compilation-start (format "make%s %s" verboseness target) nil ))))

(defun boa-run-query (query)
  "Run the Boa query QUERY."
  (interactive (list (progn (boa--parse-study-config)
                            (completing-read "Query: " boa-project-query-outputs nil t))))
  (boa-compile query))

(defun boa-run-csv (csv)
  "Generate csv file CSV."
  (interactive (list (progn (boa--parse-study-config)
                            (completing-read "CSV: " boa-project-csv-outputs nil t))))
  (boa-compile csv))

(defun boa-run-analysis (analysis)
  "Run the analysis ANALYSIS."
  (interactive (list (progn (boa--parse-study-config)
                            (completing-read "Analysis: " boa-project-query-analyses nil t))))
  (boa-compile analysis))


;;; Boa Mode Map
(defvar boa-mode-map
  (let ((map (c-make-inherited-keymap)))
    map))

(define-key boa-mode-map (kbd "C-c C-r q") #'boa-run-query)
(define-key boa-mode-map (kbd "C-c C-r c") #'boa-run-csv)
(define-key boa-mode-map (kbd "C-c C-r a") #'boa-run-analysis)


;;; Mode definition

;;;###autoload
(define-derived-mode boa-mode c-mode "Boa"
  "Boa Mode is a major mode for editing Boa language files."
  :syntax-table boa-mode-syntax-table
  :abbrev-table boa-mode-abbrev-table
  (progn
    (setq comment-start "# "
          comment-end ""
          font-lock-defaults boa-mode-font-lock-keywords
          tab-width 4
          c-basic-offset 4
          indent-tabs-mode nil)
    (c-set-offset 'label 0)
    (c-set-offset 'substatement-label 0)
    (c-set-offset 'case-label 0)
    (c-set-offset 'access-label 0)
    (setq-local boa-project-dir (cdr (project-current)))
    (when (stringp boa-project-dir)
      (setq-local boa-file-relative-name (file-relative-name (buffer-file-name) (expand-file-name "boa" boa-project-dir))))
    (when (and (stringp boa-project-dir)
             (file-exists-p (boa--study-config-file))
             (fboundp 'json-parse-buffer))
      (setq-local boa-project-study-config-p (file-attribute-modification-time (file-attributes (boa--study-config-file))))
      (boa--parse-study-config))
    (c-run-mode-hooks 'c-mode-common-hook)))

(add-to-list 'auto-mode-alist '("\\.boa\\'" . boa-mode))

(provide 'boa-mode)

;;; boa-mode.el ends here
