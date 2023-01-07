;;; boa-doc.el --- Eldoc support for the Boa language  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 2.0.0
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
  (let ((table (obarray-make)))
    (mapc #'(lambda (function-doc)
              (set (intern (upcase (boa-doc-function-data-name function-doc)) table) function-doc))
          (list))
    table)
  "Documentation data for Boa in annotated format.")

(defvar boa-doc-documentation-table
  (let ((table (obarray-make)))
    (mapc #'(lambda (x)
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
