;;; boa-mode.el --- Mode for boa language files

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.4.2
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


;;; Boa Support for Eldoc

(defvar boa-eldoc-docs-table
  (let ((table (obarray-make)))
    (mapcar #'(lambda (x)
                (cl-destructuring-bind (item description) x
                  (set (intern (upcase item) table) description)))
            '(;; Built-in Functions
              ("sort" "(t: time, n: int [, timezone: string]): time")
              ("addday" "(t: time, n: int [, timezone: string]): time")
              ("addmonth" "(t: time, n: int [, timezone: string]): time")
              ("addweek" "(t: time, n: int [, timezone: string]): time")
              ("addyear" "(t: time, n: int [, timezone: string]): time")
              ("dayofmonth" "(t: time [, timezone: string]): int")
              ("dayofweek" "(t: time [, timezone: string]): int")
              ("dayofyear" "(t: time [, timezone: string]): int")
              ("formattime" "(format: string, t: time [, timezone: string]): string")
              ("hourof" "(t: time [, timezone: string]): int")
              ("minuteof" "(t: time [, timezone: string]): int")
              ("monthof" "(t: time [, timezone: string]): int")
              ("now" "(): time")
              ("secondof" "(t: time [, timezone: string]): int")
              ("trunctoday" "(t: time [, timezone: string]): time")
              ("trunctohour" "(t: time [, timezone: string]): time")
              ("trunctominute" "(t: time [, timezone: string]): time")
              ("trunctomonth" "(t: time [, timezone: string]): time")
              ("trunctoyear" "(t: time [, timezone: string]): time")
              ("yearof" "(t: time [, timezone: string]): int")
              ("clear" "(t: map[key_type] of val_type) (or set, queue, stack)")
              ("clone" "(t: map[key_type] of val_type): map[key_type] of val_type (or set, queue, stack)")
              ("haskey" "(m:map[key_type] of val_type, key: key_type): bool")
              ("keys" "(m: map[key_type] of val_type): array of key_type")
              ("lookup" "(m: map[key_type] of val_type, key: key_type, value: val_type): val_type")
              ("remove" "(m: map[key_type] of val_type, k: key_type) (or set)")
              ("values" "(m: map[key_type] of val_type): array of val_type (or set, queue, stack)")
              ("abs" "(x: type): type (type is int or float)")
              ("acos" "(x: float): float")
              ("acosh" "(x: float): float")
              ("asin" "(x: float): float")
              ("asinh" "(x: float): float")
              ("atan" "(x: float): float")
              ("atanh" "(x: float): float")
              ("atan2" "(x: float): float")
              ("ceil" "(x: float): float")
              ("cos" "(x: float): float")
              ("cosh" "(x: float): float")
              ("exp" "(x: float): float")
              ("floor" "(x: float): float")
              ("highbit" "(n: int): int")
              ("isfinite" "(n: float): bool")
              ("isinf" "(n: float): bool")
              ("isnan" "(n: float): bool")
              ("isnormal" "(n: float): bool")
              ("log" "(x: float): float")
              ("log10" "(x: float): float")
              ("max" "(v1: type, v2: type): type (type is int, time, string, float)")
              ("min" "(v1: type, v2: type): type (type is int, time, string, float)")
              ("nrand" "(n: int): int")
              ("pow" "(x: float, y: float): float")
              ("sin" "(x: float): float")
              ("sinh" "(x: float): float")
              ("sqrt" "(x: float): float")
              ("tan" "(x: float): float")
              ("tanh" "(x: float): float")
              ("trunc" "(x: float): float")
              ("def" "(v: any_type): bool")
              ("hash" "(v: any_type): int")
              ("len" "(v: any_type): int")
              ("offer" "(q: queue of val_type, val: val_type)")
              ("peek" "(q: queue/stack of val_type): val_type")
              ("poll" "(q: queue of val_type): val_type")
              ("add" "(s: set of val_type, v: val_type)")
              ("contains" "(s: set of val_type, v: val_type): bool")
              ("containsall" "(s1: set of val_type, s2: set of val_type): bool")
              ("difference" "(s1: set of val_type, s2: set of val_type): set of val_type")
              ("intersect" "(s1: set of val_type, s2: set of val_type): set of val_type")
              ("symdiff" "(s1: set of val_type, s2: set of val_type): set of val_type")
              ("union" "(s1: set of val_type, s2: set of val_type): set of val_type")
              ("pop" "(s: stack of val_type): val_type")
              ("push" "(s: stack of val_type, val: val_type)")
              ("format" "(format: string, args: string, ...): string")
              ("lowercase" "(str: string): string")
              ("match" "(regex: string, str: string): bool")
              ("matchposns" "(regex: string, str: string): array of int")
              ("matchstrs" "(regex: string, str: string): array of string")
              ("regex" "(t: any_type, base: int): string")
              ("split" "(s: string, regex: string): array of string")
              ("splitn" "(n: int, s: string, regex: string): array of string")
              ("strfind" "(needle: string, haystack: string): int")
              ("strreplace" "(haystack: string, needle: string, replacement: string, replace_all: bool): string")
              ("strrfind" "(needle: string, haystack: string): int")
              ("substr" "substring (str: string, start: int [, end: int] ): string")
              ("trim" "(str: string): string")
              ("uppercase" "(str: string): string")
              ("bool" "(v: basic_type): bool")
              ("float" "(v: basic_type): float")
              ("int" "(v: basic_type): int")
              ("string" "(v: basic_type): string")
              ("time" "(v: basic_type): time")
              ;; Domain Specific Functions
              ("getast" "(file: ChangedFile): ASTRoot")
              ("getsnapshot" "(cr: CodeRepository [, t: time] [, filters: string...]): array of ChangedFile")
              ("hasfiletype" "(data: dsl_type, extension: string): bool")
              ("isfixingrevision" "(log: string/Revision): bool")
              ("iskind" "(s: string, k: dsl_type): bool")
              ("isliteral" "(e: Expression, s: string): bool")
              ("dot" "(g: graph): string")
              ("getcdg" "(m: Method): CDG")
              ("getcfg" "(m: Method): CFG")
              ("getddg" "(m: Method): DDG")
              ("getinedge" "(node1: graph_node, node2: graph_node): graph_edge")
              ("getoutedge" "(node1: graph_node, node2: graph_node): graph_edge")
              ("getpdg" "(m: Method): PDG")
              ("getvalue" "(n: graph_node [, t: traversal]): T")))
    table)
  "Documentation for default functions in the Boa language.")

;; Taken from https://www.emacswiki.org/emacs/c-eldoc.el c-eldoc-function-and-argument with slight modification
(defun boa-get-current-function (&optional limit)
  "Get the name of the current function, subject to LIMIT."
  (let* ((literal-limits (c-literal-limits))
         (literal-type (c-literal-type literal-limits)))
    (save-excursion
      ;; if this is a string, move out to function domain
      (when (eq literal-type 'string)
        (goto-char (car literal-limits))
        (setq literal-type nil))
      (if literal-type
          nil
        (c-save-buffer-state ((argument-index 1))
          (while (or (eq (c-forward-token-2 -1 t limit) 0)
                    (when (eq (char-before) ?\[)
                      (backward-char)
                      t))
            (when (eq (char-after) ?,)
              (setq argument-index (1+ argument-index))))
          (c-backward-syntactic-ws)
          (when (eq (char-before) ?\()
            (backward-char)
            (c-forward-token-2 -1)
            (when (looking-at "[a-zA-Z_][a-zA-Z_0-9]*")
              (buffer-substring-no-properties
               (match-beginning 0) (match-end 0)))))))))

(defun boa-eldoc-function ()
  "Return documentation for functions."
  (if-let ((current-function-name (boa-get-current-function))
           (documentation-string (symbol-value (intern-soft (upcase current-function-name) boa-eldoc-docs-table))))
      (concat (propertize current-function-name 'face 'font-lock-function-name-face) ": " documentation-string)
    ""))


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
    (setq-local eldoc-documentation-function #'boa-eldoc-function)
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
