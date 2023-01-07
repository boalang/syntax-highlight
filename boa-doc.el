;;; boa-doc.el --- Eldoc support for the Boa language  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 2.0.1
;; Package-Requires: ((boa-mode "1.4.3") (cc-mode "5.33.1"))
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

(defvar boa-doc-documentation-data
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
                                         :type "Method")))
    (mapc #'(lambda (function-doc)
              (set (intern (upcase (boa-doc-function-data-name function-doc)) table) function-doc))
          (list
           ;; Built-in Functions
           ;; ("sort" "(t: time, n: int [, timezone: string]): time")
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
           ;; ("clear" "(t: map[key_type] of val_type) (or set, queue, stack)")
           ;; ("clone" "(t: map[key_type] of val_type): map[key_type] of val_type (or set, queue, stack)")
           ;; ("haskey" "(m:map[key_type] of val_type, key: key_type): bool")
           ;; ("keys" "(m: map[key_type] of val_type): array of key_type")
           ;; ("lookup" "(m: map[key_type] of val_type, key: key_type, value: val_type): val_type")
           ;; ("remove" "(m: map[key_type] of val_type, k: key_type) (or set)")
           ;; ("values" "(m: map[key_type] of val_type): array of val_type (or set, queue, stack)")
           (make-boa-doc-function :name "highbit"
                                  :return-type "int"
                                  :arguments (list int-n)
                                  :documentation "Return the position of the high bit.")
           (make-boa-doc-function :name "nrand"
                                  :return-type "int"
                                  :arguments (list int-n))
           (make-boa-doc-function :name "abs"
                                  :return-type "type (int/float)"
                                  :arguments (list (make-boa-doc-argument :name "x"
                                                                          :type "type")))
           (make-boa-doc-function :name "isfinite"
                                  :return-type "bool"
                                  :arguments (list x-float))
           (make-boa-doc-function :name "isinf"
                                  :return-type "bool"
                                  :arguments (list x-float))
           (make-boa-doc-function :name "isnan"
                                  :return-type "bool"
                                  :arguments (list x-float))
           (make-boa-doc-function :name "isnormal"
                                  :return-type "bool"
                                  :arguments (list x-float))

           ;; ("max" "(v1: type, v2: type): type (type is int, time, string, float)")
           ;; ("min" "(v1: type, v2: type): type (type is int, time, string, float)")

           ;; ("pow" "(x: float, y: float): float")
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
           ;; ("offer" "(q: queue of val_type, val: val_type)")
           ;; ("peek" "(q: queue/stack of val_type): val_type")
           ;; ("poll" "(q: queue of val_type): val_type")
           ;; ("add" "(s: set of val_type, v: val_type)")
           ;; ("contains" "(s: set of val_type, v: val_type): bool")
           ;; ("containsall" "(s1: set of val_type, s2: set of val_type): bool")
           ;; ("difference" "(s1: set of val_type, s2: set of val_type): set of val_type")
           ;; ("intersect" "(s1: set of val_type, s2: set of val_type): set of val_type")
           ;; ("symdiff" "(s1: set of val_type, s2: set of val_type): set of val_type")
           ;; ("union" "(s1: set of val_type, s2: set of val_type): set of val_type")
           ;; ("pop" "(s: stack of val_type): val_type")
           ;; ("push" "(s: stack of val_type, val: val_type)")
           ;; ("format" "(format: string, args: string, ...): string")
           ;; ("lowercase" "(str: string): string")
           ;; ("match" "(regex: string, str: string): bool")
           ;; ("matchposns" "(regex: string, str: string): array of int")
           ;; ("matchstrs" "(regex: string, str: string): array of string")
           ;; ("regex" "(t: any_type, base: int): string")
           ;; ("split" "(s: string, regex: string): array of string")
           ;; ("splitn" "(n: int, s: string, regex: string): array of string")
           ;; ("strfind" "(needle: string, haystack: string): int")
           ;; ("strreplace" "(haystack: string, needle: string, replacement: string, replace_all: bool): string")
           ;; ("strrfind" "(needle: string, haystack: string): int")
           ;; ("substr" "substring (str: string, start: int [, end: int] ): string")
           ;; ("trim" "(str: string): string")
           ;; ("uppercase" "(str: string): string")
           ;; ("bool" "(v: basic_type): bool")
           ;; ("float" "(v: basic_type): float")
           ;; ("int" "(v: basic_type): int")
           ;; ("string" "(v: basic_type): string")
           ;; ("time" "(v: basic_type): time")
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
                                                                               :type "string"
                                                                               ))
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
                                       :arguments (:list method-m)
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
                                       "Perform traversal t starting at node n returning value.")))
    table)
  "Documentation data for Boa in rich-data format.")

(defvar boa-doc-documentation-table
  (let ((table (obarray-make)))
    (mapc #'(lambda (x)
              (cl-destructuring-bind (item description) x
                (set (intern (upcase item) table) description)))
          '(;; Built-in Functions
            ("sort" "(t: time, n: int [, timezone: string]): time")
            ("clear" "(t: map[key_type] of val_type) (or set, queue, stack)")
            ("clone" "(t: map[key_type] of val_type): map[key_type] of val_type (or set, queue, stack)")
            ("haskey" "(m:map[key_type] of val_type, key: key_type): bool")
            ("keys" "(m: map[key_type] of val_type): array of key_type")
            ("lookup" "(m: map[key_type] of val_type, key: key_type, value: val_type): val_type")
            ("remove" "(m: map[key_type] of val_type, k: key_type) (or set)")
            ("values" "(m: map[key_type] of val_type): array of val_type (or set, queue, stack)")
            ("max" "(v1: type, v2: type): type (type is int, time, string, float)")
            ("min" "(v1: type, v2: type): type (type is int, time, string, float)")
            ("pow" "(x: float, y: float): float")
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
            ("time" "(v: basic_type): time")))
    table)
  "Documentation for default functions in the Boa language.")

;; Taken from https://www.emacswiki.org/emacs/c-eldoc.el c-eldoc-function-and-argument with slight modification
(defun boa-get-current-function (&optional limit)
  "Get the name of the current function, subject to LIMIT."
  (let* ((literal-limits (c-literal-limits))
         (literal-type (c-literal-type literal-limits))
         (argument-index 0))
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

(defun boa-doc-format-info (function-name arg-index)
  "Format documentation for FUNCTION-NAME, given current ARG-INDEX."
  (if-let ((doc-object (symbol-value (intern-soft (upcase function-name) boa-doc-documentation-data))))
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
                               (incf i)))
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
                ""))
    (message "Defaulted...")
    (symbol-value (intern-soft (upcase function-name) boa-doc-documentation-table))))

(defun boa-doc-function (callback)
  "Display documentation about current function through CALLBACK.

Current function is determined by `boa-get-current-function', and
formatted with `boa-doc-format-info'."
  (when-let ((current-function-info (boa-get-current-function))
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
  :interactive (list 'boa-mode)
  (if boa-doc-mode
      (progn
        (eldoc-mode +1)
        (add-to-list (make-local-variable 'eldoc-documentation-functions) #'boa-doc-function))
    (setq-local eldoc-documentation-functions (remove #'boa-doc-function eldoc-documentation-functions))))

;;;###autoload
(add-hook 'boa-mode-hook #'boa-doc-mode)

(provide 'boa-doc)

;;; boa-doc.el ends here
