;;; boa-study-config.el --- Mode for boa language files  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 2.3.3
;; Package-Requires: ((emacs "25.1") (boa-sc-data "1.1.0") (json-snatcher "1.0") (json-mode "0.2") (project "0.8.1"))
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

;; This package provides further support for the Boa Study Template
;; (https://github.com/boalang/study-template).

(require 'boa-sc-data)
(require 'cl-lib)
(require 'json-snatcher)
(require 'json-mode)

;;; Code:


;;; Mode variables

(defvar-local boa-study-config-project-dir nil
  "Location of the Boa Study Config project.")


;;; Current Context

(defun boa-study-config--get-path ()
  "Get a JSON path and remove spurious quotation marks."
  (mapcar (lambda (x)
            (if (stringp x)
                (substring x 1 (1- (length x)))
              (format "%d" x)))
          (jsons-get-path)))

(defun boa-study-config-current-context ()
  "Determine context at point using JSON path.

Possible contexts include:

 - `:query-fn' the filename of a Boa query.
 - `:output-fn' the filename of an output (plain text).
 - `:substitution-fn' the filename of a substitution.
 - `:inputs-list' the filename of an input to an analysis.
 - `:dataset-name' the name of a dataset.
 - `:processor-output' the filename to the output of a processor.
 - `:processor-fn' the filename of a processor.
 - `:csv-as-fn' the filename of a CSV.
 - `:csv-output' the filename of a CSV in a more complex CSV generation
   structure.
 - `:analysis-fn' the filename of an analysis script."
  (when-let ((path (boa-study-config--get-path)))
    (cond
     ((string= (nth 0 path) "query")
      :query-fn)
     ((and (string= (car (last path)) "queries")
           (= (length path) 2))
      :output-fn)
     ((and (string= (nth 0 path) "file")
           (string= (nth (1- (length path)) path) "substitutions"))
      :substitution-fn)
     ((and
       (string= (nth 1 path) "input"))
      :inputs-list)
     ((string= (nth 0 path) "dataset")
      :dataset-name)
     ((and (cl-member "processors" path :test #'string=)
           (string= (nth 0 path) "output"))
      :processor-output)
     ((string= (nth 1 path) "processors")
      :processor-fn)
     ((and
       (string= (nth 0 path) "csv"))
      :csv-as-fn)
     ((and (cl-member "csv" path :test #'string=)
           (string= (nth 0 path) "output"))
      :csv-output)
     ((cl-member "analyses" path :test #'string=)
      :analysis-fn))))


;;; Completion Commands

(defun boa-study-config-completion-at-point ()
  "Determine possible completions.

Candidates are determined using `boa-study-config-current-context'.
Applicable contexts (and completions) are:

 - `:query-fn' .boa files from boa/queries.
 - `:substitution-fn' .boa files from boa/snippets.
 - `:inputs-list' declared outputs and CSVs within the file.
 - `:processor-fn' .py files in bin/.
 - `:analysis-fn' .py files in analyses/ not in analyses/common/.
 - `:dataset-name' declared datasets within the file."
  (let* ((completions
          (pcase (boa-study-config-current-context)
            (:query-fn
             (mapcar #'(lambda (x) (substring x 4))
                     (directory-files-recursively "boa/queries" ".*\\.boa$")))
            (:substitution-fn
             (mapcar #'(lambda (x) (substring x 13))
                     (directory-files-recursively "boa/snippets" ".*\\.boa$")))
            (:inputs-list
             (append (boa-sc-outputs boa-study-config-project-dir)
                     (boa-sc-csvs boa-study-config-project-dir)))
            (:processor-fn
             (mapcar #'(lambda (x) (substring x 4))
                     (directory-files-recursively "bin/" ".*\\.py$")))
            (:analysis-fn
             (cl-remove-if (apply-partially 'string-prefix-p "common/")
                           (mapcar #'(lambda (x) (substring x 9))
                                   (directory-files-recursively "analyses/" ".*\\.py$"))))
            (:dataset-name
             (boa-sc-datasets boa-study-config-project-dir))))
         (bounds (bounds-of-thing-at-point 'filename))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point))))
    (unless (null completions)
      (list start
            end
            completions))))

(defun boa-study-config-runnable-at-point ()
  "Determine if there is a runnable at point, and return it."
  (interactive)
  (pcase (boa-study-config-current-context)
    ((or :processor-output
         :csv-as-fn
         :csv-output
         :inputs-list
         :output-fn)
     (thing-at-point 'filename))
    (:analysis-fn
     (let ((file-name (car (last (butlast (jsons-get-path))))))
       (file-name-sans-extension (substring file-name 1 (1- (length file-name))))))))


;;; Build Commands

(defun boa-study-config-run (target)
  "Select and run TARGET from Study Config.

Default for TARGET is determined using `boa-study-config-runnable-at-point'."
  (interactive (completing-read "Study Config Target? "
                                (append (mapcar #'(lambda (x) (format "data/txt/%s" x)) (boa-sc-outputs boa-study-config-project-dir))
                                        (mapcar #'(lambda (x) (format "data/csv/%s" x)) (boa-sc-csvs boa-study-config-project-dir))
                                        (mapcar #'file-name-sans-extension (boa-sc-analyses boa-study-config-project-dir)))
                                nil t (boa-study-config-runnable-at-point)))
  (boa-sc-compile boa-study-config-project-dir target))


;;; Commands

(defun boa-study-config-ffap-file (file-at-point)
  "Determine the current file from the FILE-AT-POINT.

Possibilities are determined from `boa-study-config-current-context',
and follow patterns similar to completion."
  (pcase (boa-study-config-current-context)
    (:query-fn
     (format "boa/%s" file-at-point))
    (:substitution-fn
     (format "boa/snippets/%s" file-at-point))
    (:inputs-list
     (format (if (string-suffix-p "csv" file-at-point) "data/csv/%s" "data/txt/%s") file-at-point))
    (:processor-output file-at-point)
    (:processor-fn (format "bin/%s" file-at-point))
    ((or :csv-as-fn :csv-output) (format "data/csv/%s" file-at-point))
    (:output-fn (format "data/txt/%s" file-at-point))
    (:analysis-fn (format "analyses/%s" file-at-point))))

(defun boa-study-config-ffap ()
  "Open the file at point.

Note: Use `find-file-at-point' (\\[find-file-at-point]) instead."
  (interactive)
  (find-file (boa-study-config-ffap-file (thing-at-point 'filename t))))


;;; Mode definition

(defvar boa-study-config-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (binding)
              (cl-destructuring-bind (binding function) binding
                (define-key map (kbd binding) function)))
          '(("C-c C-s v" boa-sc-set-verbose)
            ("C-c C-s f" boa-study-config-ffap)
            ("C-c C-c" boa-study-config-run)))
    map)
  "Keymap for `boa-study-config-mode'.")

(defun boa-study-config-maybe-enable ()
  "Conditionally enable `boa-study-config-mode'.

The study-config mode will be enabled when in `json-mode', and if
the filename is \"study-config.json\"."
  (when (and (derived-mode-p 'json-mode)
             (string-match-p "study-config.json" (buffer-file-name)))
    (message "Enabling Boa Study Config Mode.")
    (boa-study-config-mode)))

(defun boa-study-config-insinuate ()
  "Add `boa-study-config-maybe-enable' to `json-mode-hook'."
  (add-hook 'json-mode-hook #'boa-study-config-maybe-enable))

(defun boa-study-config-reparse ()
  "Reparse Boa study config on save."
  (boa-sc-parse boa-study-config-project-dir))

(define-minor-mode boa-study-config-mode
  "Provide support for editing Boa \"study-config.json\" files.

Support includes completion, compilation/run, file opening, and
integration with `find-file-at-point'.

\\{boa-study-config-mode-map}"
  :lighter " Boa/SC"
  :interactive t
  :keymap boa-study-config-mode-map
  (when boa-study-config-mode
    (add-hook 'after-save-hook #'boa-study-config-reparse -100 t)
    (setq-local tab-width 2
                js-indent-level 2
                indent-tabs-mode nil
                boa-study-config-project-dir (boa-sc-get-project-dir)
                ffap-alist (cons '(json-mode . boa-study-config-ffap-file) ffap-alist)
                completion-at-point-functions (cons 'boa-study-config-completion-at-point completion-at-point-functions))))

(provide 'boa-study-config)

;;; boa-study-config.el ends here
