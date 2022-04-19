;;; boa-mode.el --- Mode for boa language files


;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.0
;; Package-Requires: ((cc-mode "5.33.1"))
;; Keywords: boa, msr, language
;; URL: https://github.com/boalang/syntax-highlight

;;; Commentary:
;;

;; This package provides basic language support for Boa
;; (https://boa.cs.iastate.edu).  More features are coming.

(require 'cc-langs)

;;; Code:

(defvar boa-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?# "<. " table)
    (modify-syntax-entry ?\n "> " table)
    table)
  "Syntax table for `boa-mode'.")

(defvar boa-errors'("goto" "const")
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

(defvar boa-builtins '("acosh"
                       "addday"
                       "addmonth"
                       "addweek"
                       "addyear"
                       "asinh"
                       "assert"
                       "assignlatestvalue"
                       "atanh"
                       "cnf"
                       "collect_annotations"
                       "collect_generic_types"
                       "converttoarray"
                       "converttosymbolicname"
                       "dayofmonth"
                       "dayofweek"
                       "dayofyear"
                       "debug"
                       "def"
                       "dnf"
                       "dot"
                       "format"
                       "formattime"
                       "get_annotation"
                       "getast"
                       "getastcount"
                       "getcdg"
                       "getcfg"
                       "getcfgslice"
                       "getcomments"
                       "getcrypthash"
                       "getddg"
                       "getissues"
                       "getlang"
                       "get_metric_noa"
                       "get_metric_noc"
                       "get_metric_noo"
                       "get_metric_npm"
                       "getnoargsvariables"
                       "get_nodes_with_definition"
                       "getpdg"
                       "getpdgslice"
                       "getpdtree"
                       "getpreviousversion"
                       "getrevision"
                       "getrevisionindex"
                       "getrevisionindex2"
                       "getrevisionscount"
                       "getsnapshot"
                       "getsnapshotbyid"
                       "getsnapshotbyindex"
                       "get_variable_def"
                       "get_variable_killed"
                       "get_variable_used"
                       "has_annotation"
                       "hasfiletype"
                       "has_modifier"
                       "has_modifier_final"
                       "has_modifier_namespace"
                       "has_modifier_private"
                       "has_modifier_protected"
                       "has_modifier_public"
                       "has_modifier_static"
                       "has_modifier_synchronized"
                       "has_visibility"
                       "hourof"
                       "isboollit"
                       "ischarlit"
                       "isfinite"
                       "isfixingrevision"
                       "isfloatlit"
                       "isintlit"
                       "iskind"
                       "isliteral"
                       "isnan"
                       "isnormal"
                       "isnulllit"
                       "isstringlit"
                       "istypelit"
                       "join"
                       "lowercase"
                       "match"
                       "matchposns"
                       "matchstrs"
                       "minuteof"
                       "monthof"
                       "new"
                       "nnf"
                       "normalize"
                       "now"
                       "nrand"
                       "parse"
                       "parseexpression"
                       "prettyprint"
                       "push"
                       "pop"
                       "rand"
                       "reduce"
                       "secondof"
                       "setlang"
                       "simplify"
                       "sort"
                       "split"
                       "splitn"
                       "strfind"
                       "strreplace"
                       "strrfind"
                       "substring"
                       "trim"
                       "trunc"
                       "trunctoday"
                       "trunctohour"
                       "trunctominute"
                       "trunctomonth"
                       "trunctosecond"
                       "trunctoyear"
                       "type_name"
                       "uppercase"
                       "url"
                       "yearof")
  "List of builtins for `boa-mode'.")

(defvar boa-mode-font-lock-keywords
  `((("\"\\.\\*\\?" . font-lock-string-face)
     (,(regexp-opt boa-errors 'word) . font-lock-warning-face)
     (,(regexp-opt boa-keywords 'word) . font-lock-keyword-face)
     (,(regexp-opt boa-constants 'word) . font-lock-constant-face)
     (,(regexp-opt boa-types 'word) . font-lock-type-face)
     (,(regexp-opt boa-builtins 'word) . font-lock-builtin-face)))
  "Boa font-locking configuration.")

;;;###autoload
(define-derived-mode boa-mode c-mode "Boa"
  "Boa Mode is a major mode for editing Boa language files."
  :syntax-table boa-mode-syntax-table
  (setq comment-start "# "
        comment-end ""
        font-lock-defaults boa-mode-font-lock-keywords
        tab-width 4
        c-basic-offset 4
        indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.boa\\'" . boa-mode))

(provide 'boa-mode)

;;; boa-mode.el ends here
