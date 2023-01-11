;;; boa-mode.el --- Mode for boa language files  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 2.2.0
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

(require 'cc-mode)
(require 'cc-langs)
(require 'cc-cmds)
(require 'cc-styles)
(require 'easymenu)
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

;; TODO: Read keywords, etc. from JSON file.

(defvar boa-errors '("goto" "const")
  "List of error keywords in the Boa language..")

(defvar boa-keywords '("if" "else" "switch" "before" "after" "case" "default"
                       "while" "for" "do" "foreach" "exists" "ifall" "visit" "function"
                       "return" "stop" "visit" "break" "continue" "skipwhite")
  "List of keywords for the Boa language.")

(defvar boa-constants '("true" "false" "input")
  "List of known constants in the Boa language.")

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
  "List of builtins in the Boa language.")

(defvar boa-mode-font-lock-keywords
  `((("\"\\.\\*\\?" . font-lock-string-face)
     (,(regexp-opt boa-errors 'symbols) . font-lock-warning-face)
     (,(regexp-opt boa-keywords 'symbols) . font-lock-keyword-face)
     (,(regexp-opt boa-constants 'symbols) . font-lock-constant-face)
     (,(regexp-opt boa-types 'symbols) . font-lock-type-face)
     (,(regexp-opt boa-builtins 'symbols) . font-lock-builtin-face)))
  "Keywords used to font-lock `boa-mode'.")


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
     (:else (buffer-file-name)))))
  "Location of pre-build snippets for `boa-mode'.")

(defun boa-mode-enable-snippets ()
  "Load snippets for Boa mode."
  (add-to-list 'yas-snippet-dirs boa-mode-snippets-dir t)
  (yas-load-directory boa-mode-snippets-dir t))

(with-eval-after-load 'yasnippet
  (boa-mode-enable-snippets))


;; Autocompletion

(defvar boa-symbol-regex (rx (+ (or (syntax word) (syntax symbol))))
  "Syntax for Boa symbols.")

(defun boa-scan-names ()
  ;; TODO: Support scope detection
  "Scan file for names of definitions."
  (let ((names (list)))
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward (rx (seq (group-n 1 (regex boa-symbol-regex))
                                           (* (syntax whitespace)) ":"))
                                  nil t)
          (cl-pushnew (match-string-no-properties 1) names :test #'string=))))
    names))


(defun boa-scan-type-names ()
  "Scan buffer for names of types."
  (let ((names (list)))
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward (rx (seq "type" (* (syntax whitespace))
                                           (group-n 1 (regex boa-symbol-regex))
                                           (* (syntax whitespace)) "="))
                                  nil t)
          (cl-pushnew (match-string-no-properties 1) names :test #'string=))))
    names))

(defun boa-autocomplete-symbol ()
  "Autocompletion provider for `boa-mode'.

Uses `boa-keywords', `boa-types', and `boa-builtins'."
  (let* ((symbol-bounds (bounds-of-thing-at-point 'symbol))
         (symbol-start (car symbol-bounds))
         (symbol-end (cdr symbol-bounds)))
    (list symbol-start
          symbol-end
          (append boa-keywords
                  boa-types
                  boa-builtins
                  (boa-scan-names)
                  (boa-scan-type-names)))))


;;; Mode definition
(defun boa-update-modeline (original)
  "Update modeline, advice around `c-update-modeline' (ORIGINAL).

In addition to basic `c-mode' mode line configuration, if
`boa-ide-mode' is enabled, modify the lighter (show \"(IDE)\")."
  (if (derived-mode-p 'boa-mode)
      (let ((fmt (format "/%s%s%s%s%s%s%s"
		         (if c-block-comment-flag "*" "/")
		         (if c-electric-flag "l" "")
		         (if (and c-electric-flag c-auto-newline)
			     "a" "")
		         (if c-hungry-delete-key "h" "")
		         (if (and
			      ;; (cc-)subword might not be loaded.
			      (boundp 'c-subword-mode)
			      (symbol-value 'c-subword-mode))
                             ;; FIXME: subword-mode already comes with its
                             ;; own lighter!
			     "w"
		           "")
                         (if (bound-and-true-p boa-doc-mode) "d" "")
                         (if (bound-and-true-p boa-ide-mode)
                             "(IDE)" "")))
            (bare-mode-name (if (string-match "\\(^[^/]*\\)/" mode-name)
			        (match-string 1 mode-name)
			      mode-name)))

        (setq mode-name
	      (if (> (length fmt) 1)
	          (concat bare-mode-name fmt)
	        bare-mode-name))
        (force-mode-line-update))
    (funcall original)))
(advice-add 'c-update-modeline :around #'boa-update-modeline)

(defvar boa-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap for Boa Mode.")

(put 'boa-mode 'c-mode-prefix "boa-")
(c-add-language 'boa-mode 'c-mode)

(easy-menu-define boa-menu boa-mode-map "Boa Mode Commands"
  (cons "Boa" (c-lang-const c-mode-menu boa)))

;; menu-title regexp index function arguments

(defvar boa-generic-expression-imenu
  `(("Functions" ,(rx (seq bol (* (syntax whitespace))
                           (group-n 1 (regex boa-symbol-regex))
                           (* (syntax whitespace)) ":=" (* (syntax whitespace))
                           "function"))
     1)
    ("Types" ,(rx (seq bol (* (syntax whitespace)) "type" (* (syntax whitespace))
                       (group-n 1 (regex boa-symbol-regex))
                       (* (syntax whitespace)) "="))
     1)
    ("Outputs" ,(rx (seq bol (* (syntax whitespace))
                         (group-n 1 (regex boa-symbol-regex))
                         (* (syntax whitespace)) ":" (* (syntax whitespace)) "output"))
     1))
  "Generic Expression for Boa Functions.

See also `imenu-generic-expression'.")

;;;###autoload
(define-derived-mode boa-mode prog-mode "Boa"
  "Boa Mode is a major mode for editing Boa language files.

Basic support is provided by `c-mode', with additional
font-locking and completion for the Boa language.  At present,
scope and name resolution is not supported.

The hook `c-mode-common-hook' is run without args, then
`boa-mode-hook'.

Key bindings:
\\{boa-mode-map}}"
  :syntax-table boa-mode-syntax-table
  :abbrev-table boa-mode-abbrev-table
  :after-hook (c-update-modeline)
  (progn
    (c-initialize-cc-mode t)
    (c-init-language-vars boa-mode)
    (c-common-init 'boa-mode)
    (setq comment-start "# "
          comment-end ""
          font-lock-defaults boa-mode-font-lock-keywords
          tab-width 4
          c-basic-offset 4
          indent-tabs-mode nil)
    (c-set-offset 'label '+)
    (c-set-offset 'substatement-label '+)
    (c-set-offset 'case-label '+)
    (c-set-offset 'access-label '+)
    (c-set-offset 'case-label 0)
    (c-set-offset 'access-label 0)
    (cc-imenu-init boa-generic-expression-imenu)
    (c-run-mode-hooks 'c-mode-common-hook)
    (setq-local completion-at-point-functions (cons 'boa-autocomplete-symbol completion-at-point-functions))))

(add-to-list 'auto-mode-alist '("\\.boa\\'" . boa-mode))

(provide 'boa-mode)

;;; boa-mode.el ends here
