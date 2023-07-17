;;; boa-doc.el --- Eldoc support for the Boa language  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 4.0.0
;; Keywords: docs, languages
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

;; This package provides eldoc support for the Boa language.

(require 'cc-langs)
(require 'cc-mode)
(require 'cl-lib)
(require 'boa-mode)

;;; Code:

(defvar boa-doc-show-full-p nil
  "Should full documentation for a function be shown?")

;; TODO: Read from a JSON file

(cl-defstruct boa-doc-argument
  "Argument to a Boa function.  Name and type required, optionalp defaults to nil."
  name
  type
  (optionalp nil))

(cl-defstruct boa-doc-function-data
  "Documentation for a Boa function.

`name', `arguments' and `return-type' required.  `variadicp'
defaults to nil.

If slot `documentation', is filled and `boa-doc-show-full-p' is
t, show full documentation."
  name
  arguments
  return-type
  (variadicp nil)
  documentation)

(defvar boa-doc-documentation-table
  (let ((table (obarray-make))
        (format-string (make-boa-doc-argument :name "format"
                                              :type "string"))
        (time-t (make-boa-doc-argument :name "t"
                                       :type "time"))
        (optional-timezone (make-boa-doc-argument :name "timezone"
                                                  :type "string"
                                                  :optionalp t))
        (int-n (make-boa-doc-argument :name "n"
                                      :type "int"))
        (x-float (make-boa-doc-argument :name "x"
                                        :type "float"))
        (v-any (make-boa-doc-argument :name "v"
                                      :type "any_type"))
        (method-m (make-boa-doc-argument :name "m"
                                         :type "Method"))
        (map-m (make-boa-doc-argument :name "m"
                                      :type "map[key_type] of val_type"))
        (key (make-boa-doc-argument :name "key"
                                    :type "key_type"))
        (value-val (make-boa-doc-argument :name "value"
                                          :type "val_type"))
        (str-string (make-boa-doc-argument :name "str"
                                           :type "string"))
        (regex-string (make-boa-doc-argument :name "regex"
                                             :type "string"))
        (t-basic (make-boa-doc-argument  :name "t"
                                         :type "basic_type"))
        (queue-q (make-boa-doc-argument :name "q"
                                        :type "queue of val_type"))
        (set-s (make-boa-doc-argument :name "s"
                                      :type "set of val_type"))
        (set-s1 (make-boa-doc-argument :name "s1"
                                       :type "set of val_type"))
        (set-s2 (make-boa-doc-argument :name "s2"
                                       :type "set of val_type"))
        (t-type (make-boa-doc-argument :name "t"
                                       :type "type")))
    (mapc (lambda (function-doc)
            (set (intern (upcase (boa-doc-function-data-name function-doc)) table) function-doc))
          (list
           ;; Built-in Functions
           (make-boa-doc-function-data :name "sort"
                                       :return-type "time"
                                       :arguments (list time-t int-n optional-timezone))
           (make-boa-doc-function-data :name "addday"
                                       :return-type "time"
                                       :arguments (list time-t int-n optional-timezone)
                                       :documentation "Add n days to time t.")
           (make-boa-doc-function-data :name "addmonth"
                                       :return-type "time"
                                       :arguments (list time-t int-n optional-timezone)
                                       :documentation "Add n months to time t.")
           (make-boa-doc-function-data :name "addweek"
                                       :return-type "time"
                                       :arguments (list time-t int-n optional-timezone)
                                       :documentation "Add n weeks to time t.")
           (make-boa-doc-function-data :name "addyear"
                                       :return-type "time"
                                       :arguments (list time-t int-n optional-timezone)
                                       :documentation "Add n years to time t.")
           (make-boa-doc-function-data :name "dayofmonth"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get day of month from time t.")
           (make-boa-doc-function-data :name "dayofweek"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get day of week from time t.")
           (make-boa-doc-function-data :name "dayofyear"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get day of year from time t.")
           (make-boa-doc-function-data :name "formattime"
                                       :return-type "string"
                                       :arguments (list format-string time-t optional-timezone)
                                       :documentation "Format time t.")
           (make-boa-doc-function-data :name "hourof"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get the hour of time t.")
           (make-boa-doc-function-data :name "minuteof"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get the minute of time t.")
           (make-boa-doc-function-data :name "monthof"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get the month of time t.")
           (make-boa-doc-function-data :name "now"
                                       :return-type "time"
                                       :documentation "Get the current time.")
           (make-boa-doc-function-data :name "secondof"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get the second of time t.")
           (make-boa-doc-function-data :name "trunctoday"
                                       :return-type "time"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Truncate timestamp t to beginning of day.")
           (make-boa-doc-function-data :name "trunctohour"
                                       :return-type "time"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Truncate timestamp t to beginning of hour.")
           (make-boa-doc-function-data :name "trunctominute"
                                       :return-type "time"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Truncate timestamp t to beginning of minute.")
           (make-boa-doc-function-data :name "trunctomonth"
                                       :return-type "time"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Truncate timestamp t to beginning of month.")
           (make-boa-doc-function-data :name "trunctoyear"
                                       :return-type "time"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Truncate timestamp t to beginning of year.")
           (make-boa-doc-function-data :name "yearof"
                                       :return-type "int"
                                       :arguments (list time-t optional-timezone)
                                       :documentation "Get the year of time t.")
           (make-boa-doc-function-data :name "clear"
                                       :arguments (list t-type)
                                       :documentation "Clear collection t, where t's type is a map, set, queue or stack.")
           (make-boa-doc-function-data :name "clone"
                                       :return-type "type"
                                       :arguments (list t-type)
                                       :documentation "Clone colllection t, where t's type is a map, set, queue or stack.")
           (make-boa-doc-function-data :name "haskey"
                                       :return-type "bool"
                                       :arguments (list map-m key)
                                       :documentation "Does the map m have key?")
           (make-boa-doc-function-data :name "keys"
                                       :return-type "array of key_type"
                                       :arguments (list map-m))
           (make-boa-doc-function-data :name "lookup"
                                       :return-type "val_type"
                                       :arguments (list map-m key value-val))
           (make-boa-doc-function-data :name "remove"
                                       :arguments (list map-m key))
           (make-boa-doc-function-data :name "values"
                                       :return-type "array of val_type"
                                       :arguments (list t-type)
                                       :documentation "Return values of val_type from collection.")
           (make-boa-doc-function-data :name "highbit"
                                       :return-type "int"
                                       :arguments (list int-n)
                                       :documentation "Return the position of the high bit.")
           (make-boa-doc-function-data :name "nrand"
                                       :return-type "int"
                                       :arguments (list int-n))
           (make-boa-doc-function-data :name "abs"
                                       :return-type "type (int/float)"
                                       :arguments (list (make-boa-doc-argument :name "x"
                                                                               :type "type")))
           (make-boa-doc-function-data :name "isfinite"
                                       :return-type "bool"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "isinf"
                                       :return-type "bool"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "isnan"
                                       :return-type "bool"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "isnormal"
                                       :return-type "bool"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "max"
                                       :return-type "type (int, time, string, float)"
                                       :arguments (list (make-boa-doc-argument :name "v1"
                                                                               :type "type")
                                                        (make-boa-doc-argument :name "v2"
                                                                               :type "type")))
           (make-boa-doc-function-data :name "min"
                                       :return-type "type (int, time, string, float)"
                                       :arguments (list (make-boa-doc-argument :name "v1"
                                                                               :type "type")
                                                        (make-boa-doc-argument :name "v2"
                                                                               :type "type")))
           (make-boa-doc-function-data :name "pow"
                                       :return-type "float"
                                       :arguments (list x-float
                                                        (make-boa-doc-argument :name "y"
                                                                               :type "float")))
           (make-boa-doc-function-data :name "acos"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "acosh"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "asin"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "asinh"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "atan"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "atanh"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "atan2"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "ceil"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "cos"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "cosh"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "exp"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "floor"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "log"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "log10"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "sin"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "sinh"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "sqrt"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "tan"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "tanh"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "trunc"
                                       :return-type "float"
                                       :arguments (list x-float))
           (make-boa-doc-function-data :name "def"
                                       :return-type "bool"
                                       :arguments (list v-any)
                                       :documentation "Is v defined?")
           (make-boa-doc-function-data :name "hash"
                                       :return-type "int"
                                       :arguments (list v-any)
                                       :documentation "Hash v to an int.")
           (make-boa-doc-function-data :name "len"
                                       :return-type "int"
                                       :arguments (list v-any)
                                       :documentation "Get length of v.")
           (make-boa-doc-function-data :name "offer"
                                       :arguments (list queue-q value-val))
           (make-boa-doc-function-data :name "peek"
                                       :return-type "val_type"
                                       :arguments (list (make-boa-doc-argument :name "q"
                                                                               :type "queue/stack of val_type")))
           (make-boa-doc-function-data :name "poll"
                                       :return-type "val_type"
                                       :arguments (list queue-q))
           (make-boa-doc-function-data :name "add"
                                       :arguments (list set-s value-val))
           (make-boa-doc-function-data :name "contains"
                                       :return-type "bool"
                                       :arguments (list set-s value-val))
           (make-boa-doc-function-data :name "containsall"
                                       :return-type "bool"
                                       :arguments (list set-s1 set-s2))
           (make-boa-doc-function-data :name "difference"
                                       :return-type "set of val_type"
                                       :arguments (list set-s1 set-s2))
           (make-boa-doc-function-data :name "intersect"
                                       :return-type "set of val_type"
                                       :arguments (list set-s1 set-s2))
           (make-boa-doc-function-data :name "symdiff"
                                       :return-type "set of val_type"
                                       :arguments (list set-s1 set-s2))
           (make-boa-doc-function-data :name "union"
                                       :return-type "set of val_type"
                                       :arguments (list set-s1 set-s2))
           (make-boa-doc-function-data :name "pop"
                                       :return-type "val_type"
                                       :arguments (list set-s))
           (make-boa-doc-function-data :name "push"
                                       :arguments (list set-s value-val))
           (make-boa-doc-function-data :name "format"
                                       :return-type "string"
                                       :arguments (list format-string
                                                        (make-boa-doc-argument :name "arg"
                                                                               :type "T"
                                                                               :optionalp t))
                                       :variadicp t)
           (make-boa-doc-function-data :name "match"
                                       :return-type "bool"
                                       :arguments (list regex-string str-string))
           (make-boa-doc-function-data :name "matchposns"
                                       :return-type "array of int"
                                       :arguments (list regex-string str-string))
           (make-boa-doc-function-data :name "matchstrs"
                                       :return-type "array of string"
                                       :arguments (list regex-string str-string))
           (make-boa-doc-function-data :name "split"
                                       :return-type "array of string"
                                       :arguments (list str-string regex-string))
           (make-boa-doc-function-data :name "splitn"
                                       :return-type "array of string"
                                       :arguments (list int-n str-string regex-string))
           (make-boa-doc-function-data :name "strfind"
                                       :return-type "int"
                                       :arguments (list (make-boa-doc-argument :name "needle"
                                                                               :type "string")
                                                        (make-boa-doc-argument :name "haystack"
                                                                               :type "string")))
           (make-boa-doc-function-data :name "strrfind"
                                       :return-type "int"
                                       :arguments (list (make-boa-doc-argument :name "needle"
                                                                               :type "string")
                                                        (make-boa-doc-argument :name "haystack"
                                                                               :type "string")))
           (make-boa-doc-function-data :name "strreplace"
                                       :return-type "string"
                                       :arguments (list (make-boa-doc-argument :name "haystack"
                                                                               :type "string")
                                                        (make-boa-doc-argument :name "needle"
                                                                               :type "string")
                                                        (make-boa-doc-argument :name "replacement"
                                                                               :type "string")
                                                        (make-boa-doc-argument :name "replace_all"
                                                                               :type "bool")))
           (make-boa-doc-function-data :name "substr"
                                       :return-type "string"
                                       :arguments (list str-string
                                                        (make-boa-doc-argument :name "start"
                                                                               :type "int")
                                                        (make-boa-doc-argument :name "end"
                                                                               :type "int"
                                                                               :optionalp t)))
           (make-boa-doc-function-data :name "trim"
                                       :return-type "string"
                                       :arguments (list str-string))
           (make-boa-doc-function-data :name "lowercase"
                                       :return-type "string"
                                       :arguments (list str-string))
           (make-boa-doc-function-data :name "uppercase"
                                       :return-type "string"
                                       :arguments (list str-string))
           (make-boa-doc-function-data :name "regex"
                                       :return-type "string"
                                       :arguments (list t-type
                                                        (make-boa-doc-argument :name "base"
                                                                               :type "int")))
           (make-boa-doc-function-data :name "bool"
                                       :return-type "bool"
                                       :arguments (list t-basic))
           (make-boa-doc-function-data :name "float"
                                       :return-type "float"
                                       :arguments (list t-basic))
           (make-boa-doc-function-data :name "int"
                                       :return-type "int"
                                       :arguments (list t-basic))
           (make-boa-doc-function-data :name "string"
                                       :return-type "string"
                                       :arguments (list t-basic))
           (make-boa-doc-function-data :name "time"
                                       :return-type "time"
                                       :arguments (list t-basic))
           ;; Domain Specific Functions
           (make-boa-doc-function-data :name "getast"
                                       :return-type "ASTRoot"
                                       :arguments (list (make-boa-doc-argument :name "file"
                                                                               :type "ChangedFile")))
           (make-boa-doc-function-data :name "getsnapshot"
                                       :return-type "array of ChangedFile"
                                       :arguments (list (make-boa-doc-argument :name "cr"
                                                                               :type "CodeRepository")
                                                        (make-boa-doc-argument :name "t"
                                                                               :type "time"
                                                                               :optionalp t)
                                                        (make-boa-doc-argument :name "filters"
                                                                               :type "string"))
                                       :variadicp t
                                       :documentation "Get a snapshot of files from cr before t, subject to filters.")
           (make-boa-doc-function-data :name "hasfiletype"
                                       :return-type "bool"
                                       :arguments (list (make-boa-doc-argument :name "data"
                                                                               :type "dsl_type")
                                                        (make-boa-doc-argument :name "extension"
                                                                               :type "string")))
           (make-boa-doc-function-data :name "isfixingrevision"
                                       :return-type "bool"
                                       :arguments (list (make-boa-doc-argument :name "log"
                                                                               :type "string or Revision")))
           (make-boa-doc-function-data :name "iskind"
                                       :return-type "bool"
                                       :arguments (list (make-boa-doc-argument :name "s"
                                                                               :type "string")
                                                        (make-boa-doc-argument :name "k"
                                                                               :type "dsl_type")))
           (make-boa-doc-function-data :name "isliteral"
                                       :return-type "bool"
                                       :arguments (list (make-boa-doc-argument :name "e"
                                                                               :type "Expression")
                                                        (make-boa-doc-argument :name "s"
                                                                               :type "string")))
           (make-boa-doc-function-data :name "dot"
                                       :return-type "string"
                                       :arguments (list (make-boa-doc-argument :name "g"
                                                                               :type "graph")))
           (make-boa-doc-function-data :name "getcdg"
                                       :return-type "CDG"
                                       :arguments (list method-m)
                                       :documentation "Get CDG of method m.")
           (make-boa-doc-function-data :name "getcfg"
                                       :return-type "CFG"
                                       :arguments (list method-m)
                                       :documentation "Get CFG of method m.")
           (make-boa-doc-function-data :name "getddg"
                                       :return-type "DDG"
                                       :arguments (list method-m)
                                       :documentation "Get DDG of method m.")
           (make-boa-doc-function-data :name "getpdg"
                                       :return-type "PDG"
                                       :arguments (list method-m)
                                       :documentation "Get PDG of method m.")
           (make-boa-doc-function-data :name "getinedge"
                                       :return-type "graph_edge"
                                       :arguments (list (make-boa-doc-argument :name "node1"
                                                                               :type "graph_node")
                                                        (make-boa-doc-argument :name "node2"
                                                                               :type "graph_node"))
                                       :documentation "Get edge in from node1 to node2.")
           (make-boa-doc-function-data :name "getoutedge"
                                       :return-type "graph_edge"
                                       :arguments (list (make-boa-doc-argument :name "node1"
                                                                               :type "graph_node")
                                                        (make-boa-doc-argument :name "node2"
                                                                               :type "graph_node"))
                                       :documentation "Get edge out from node1 to node2.")
           (make-boa-doc-function-data :name "getvalue"
                                       :return-type "T"
                                       :arguments (list (make-boa-doc-argument :name "n"
                                                                               :type "graph_node")
                                                        (make-boa-doc-argument :name "t"
                                                                               :type "traversal"
                                                                               :optionalp t))
                                       :documentation "Perform traversal t starting at node n returning value.")))
    table)
  "Documentation data for Boa in rich-data format.")

;; Taken from https://www.emacswiki.org/emacs/c-eldoc.el c-eldoc-function-and-argument with slight modification
(defun boa-doc-get-current-function (&optional limit)
  "Get the name of the current function, subject to LIMIT."
  (let* ((literal-limits (c-literal-limits))
         (literal-type (c-literal-type literal-limits))
         (argument-index 0))
    (ignore argument-index)
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
              (cons (buffer-substring-no-properties
                     (match-beginning 0) (match-end 0))
                    argument-index))))))))

(defun boa-doc-scan-local-functions (&optional function-name)
  "Scan for definition of local FUNCTION-NAME."
  (interactive)
  (let ((local-doc-table (obarray-make)))
    (save-mark-and-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward (rx (seq bol (* (syntax whitespace))
                                           (group-n 1 (regex boa-symbol-regex))
                                           (* (syntax whitespace)) ":=" (* (syntax whitespace))
                                           "function" (* (syntax whitespace)) "("
                                           (group-n 2 (* any))
                                           ")" (* (syntax whitespace))
                                           (? (seq (* (syntax whitespace))
                                                   ":"
                                                   (* (syntax whitespace))
                                                   (group-n 3 (* any))))
                                           (* (syntax whitespace))
                                           "{"))
                                  nil t)
          (let ((name (match-string-no-properties 1))
                (arguments (when-let* ((arguments-string (match-string-no-properties 2))
                                       (arguments (split-string arguments-string (rx (seq "," (* (syntax whitespace)))))))
                             (mapcar (lambda (argument)
                                       (save-match-data
                                         (when (string-match
                                                (rx (seq (group-n 1 (regex boa-symbol-regex))
                                                         (* (syntax whitespace)) ":" (* (syntax whitespace))
                                                         (group-n 2 (* any))))
                                                argument)
                                           (make-boa-doc-argument :name (match-string-no-properties 1 argument)
                                                                  :type (match-string-no-properties 2 argument)))))
                                     arguments)))
                (return-type (match-string-no-properties 3)))
            (set (intern (upcase name) local-doc-table)
                 (make-boa-doc-function-data :name name
                                             :arguments arguments
                                             :return-type return-type
                                             :documentation (format "User defined function starting on line %d."
                                                                    (line-number-at-pos (match-beginning 0)))))))))
    (symbol-value (intern-soft (upcase function-name) local-doc-table))))

(defun boa-doc-format-info (function-name arg-index)
  "Format documentation for FUNCTION-NAME, given current ARG-INDEX."
  (when-let ((doc-object (or (symbol-value (intern-soft (upcase function-name) boa-doc-documentation-table))
                             (boa-doc-scan-local-functions function-name))))
    (format "(%s)%s%s"
            (let* ((arguments (boa-doc-function-data-arguments doc-object))
                   (num-args (length arguments))
                   (arg-index (1- arg-index))
                   (i 0))
              (mapconcat (lambda (argument)
                           (prog1
                               (let ((arg-string
                                      (format (if (boa-doc-argument-optionalp argument)
                                                  "[%s: %s%s]"
                                                "%s: %s%s")
                                              (propertize (boa-doc-argument-name argument)
                                                          'face 'font-lock-variable-name-face)
                                              (propertize (boa-doc-argument-type argument)
                                                          'face 'font-lock-keyword-face)
                                              (if (and (= i (1- num-args))
                                                       (boa-doc-function-data-variadicp doc-object))
                                                  "..."
                                                ""))))
                                 (when (boa-doc-argument-optionalp argument)
                                   (add-face-text-property 0 (length arg-string)
                                                           'italic
                                                           t arg-string))
                                 (when (or (= i arg-index)
                                           (and (= i (1- num-args))
                                                (>= arg-index num-args)))
                                   (add-face-text-property 0 (length arg-string)
                                                           'eldoc-highlight-function-argument
                                                           t arg-string))
                                 arg-string)
                             (cl-incf i)))
                         arguments
                         ", "))
            (if (boa-doc-function-data-return-type doc-object)
                (format ": %s" (propertize (boa-doc-function-data-return-type doc-object)
                                           'face 'font-lock-keyword-face))
              "")
            (if (and boa-doc-show-full-p (boa-doc-function-data-documentation doc-object))
                (let ((doc-string (boa-doc-function-data-documentation doc-object)))
                  (format "\n\n%s"
                          (propertize doc-string 'face 'font-lock-comment-face)))
              ""))))

(defun boa-doc-function (callback)
  "Display documentation about current function through CALLBACK.

Current function is determined by `boa-doc-get-current-function', and
formatted with `boa-doc-format-info'."
  (when-let ((current-function-info (boa-doc-get-current-function))
             (current-function-name (car current-function-info))
             (argument-index (cdr current-function-info))
             (documentation-string (boa-doc-format-info current-function-name argument-index)))
    (apply callback documentation-string
           :thing current-function-name
           :face (list 'font-lock-function-name-face))))

;;;###autoload
(define-minor-mode boa-doc-mode
  "Provide support for eldoc in Boa language files."
  :lighter " BoaDoc"
  :interactive (boa-mode)
  (if boa-doc-mode
      (progn
        (eldoc-mode +1)
        (add-to-list (make-local-variable 'eldoc-documentation-functions) 'boa-doc-function))
    (setq-local eldoc-documentation-functions (remove #'boa-doc-function eldoc-documentation-functions))))

(provide 'boa-doc)

;;; boa-doc.el ends here
