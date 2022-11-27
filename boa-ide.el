;;; boa-ide.el --- Mode for boa language files

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.4.4
;; Package-Requires: ((boa-mode "1.4.4") (cc-mode "5.33.1") (project "0.8.1"))
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

;; This package provides support for the Boa Study Template
;; (https://github.com/boalang/study-template).

(require 'project)
(require 'cl-lib)

;;; Code:


;;; Mode variables and data storage.
(defvar-local boa-ide-project-dir nil
  "Directory of current Boa project.")

(defvar-local boa-ide-file-relative-name nil
  "Relative name of Boa file.")

(defvar-local boa-ide-study-config-p nil
  "Is a Boa Study Template configuration present?

nil if a study configuration is not present, a Lisp timestamp if it is.")

(defvar-local boa-ide-study-config nil
  "Boa project study configuration.")

(defvar-local boa-ide-query-outputs nil
  "List of possible outputs for the current Boa query file.")

(defvar-local boa-ide-csv-outputs nil
  "List of possible CSV outputs for the current Boa query file.")

(defvar-local boa-ide-query-analyses nil
  "List of analyses for the current Boa query file.")

(defvar-local boa-ide-snippets nil
  "List of Snippets defined in study configuration.")

(defun boa-ide--study-config-file ()
  "Get the full path to the study configuration file."
  (expand-file-name "study-config.json" boa-ide-project-dir))



;;; Collect Study Configuration
(defun boa-ide--parse-study-config ()
  "Parse study configuration."
  (when (and (json-available-p)
           (or (null boa-ide-study-config)
              (< (time-convert boa-ide-study-config-p 'integer)
                 (time-convert (file-attribute-modification-time (file-attributes (boa-ide--study-config-file))) 'integer))))
    (message "Parsing Study Configuration.")
    (setq-local boa-ide-study-config-p (file-attribute-modification-time (file-attributes (boa-ide--study-config-file)))
                boa-ide-study-config (let ((file (boa-ide--study-config-file)))
                                       (with-temp-buffer
                                         (insert-file-contents file)
                                         (json-parse-buffer :array-type 'list))))))

(defun boa-ide-process-study-config ()
  "Process parsed study configuration to collect targets."
  (when-let ((study-configuration (boa-ide--parse-study-config)))
    (let ((current-outputs (list))
          (current-csvs (list))
          (current-snippets (list))
          (current-analyses (list)))
      (mapc #'(lambda (substitution)
                (cl-pushnew (gethash "target" substitution) current-snippets))
            (gethash "substitutions" study-configuration))
      (maphash #'(lambda (key value)
                   (when (string= boa-ide-file-relative-name (gethash "query" value))
                     (push key current-outputs)
                     (when-let ((csv-decl (gethash "csv" value)))
                       (push (gethash "output" csv-decl) current-csvs))
                     (mapc #'(lambda (substitution)
                               (cl-pushnew (gethash "target" substitution) current-snippets))
                           (gethash "substitutions" value))))
               (gethash "queries" study-configuration))
      (maphash #'(lambda (key value)
                   (when (cl-intersection (append current-outputs current-csvs)
                                          (gethash "input" value)
                                          :test #'string=)
                     (push key current-analyses)))
               (gethash "analyses" study-configuration))
      (setq-local boa-ide-query-outputs (mapcar #'(lambda (output) (format "data/txt/%s" output)) current-outputs)
                  boa-ide-csv-outputs (mapcar #'(lambda (csv) (format "data/csv/%s" csv)) current-csvs)
                  boa-ide-query-analyses (mapcar #'file-name-sans-extension current-analyses)
                  boa-ide-snippets current-snippets))))


;;; Running targets

(defvar-local boa-run-verbose nil
  "Should boa queries run with verbosity?")

(defun boa-ide-compile (target)
  "Compile TARGET from the project root directory."
  (let ((verboseness (cond
                      ((integerp boa-run-verbose)
                       (format " VERBOSE=-%s" (make-string boa-run-verbose ?v)))
                      (boa-run-verbose
                       " VERBOSE=-v")
                      (t ""))))
    (let  ((compilation-directory boa-ide-project-dir)
           (default-directory boa-ide-project-dir))
      (compilation-start (format "make%s %s" verboseness target) nil ))))

(defun boa-ide-run-query (query)
  "Run the Boa query QUERY."
  (interactive (list (progn (boa-ide-process-study-config)
                            (completing-read "Query: " boa-ide-query-outputs nil t))))
  (boa-ide-compile query))

(defun boa-ide-run-csv (csv)
  "Generate csv file CSV."
  (interactive (list (progn (boa-ide-process-study-config)
                            (completing-read "CSV: " boa-ide-csv-outputs nil t))))
  (boa-ide-compile csv))

(defun boa-ide-run-analysis (analysis)
  "Run the analysis ANALYSIS."
  (interactive (list (progn (boa-ide-process-study-config)
                            (completing-read "Analysis: " boa-ide-query-analyses nil t))))
  (boa-ide-compile analysis))


;;; Mode definition

(defvar boa-ide-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (binding)
              (cl-destructuring-bind (binding function) binding
                (define-key map (kbd binding) function)))
          '(("C-c C-r q" boa-ide-run-query)
            ("C-c C-r c" boa-ide-run-csv)
            ("C-c C-r a" boa-ide-run-analysis)))
    map)
  "Keymap for Boa IDE Support.")

(define-minor-mode boa-ide-mode
  "Provides support for Boa study template projects."
  :lighter nil
  :interactive t
  :keymap boa-ide-mode-map
  (when boa-ide-mode
    (setq-local boa-ide-project-dir (cdr (project-current)))
    (when (stringp boa-ide-project-dir)
      (setq-local boa-ide-file-relative-name
                  (file-relative-name (buffer-file-name)
                                      (expand-file-name "boa" boa-ide-project-dir)))
      (when (and (file-exists-p (boa-ide--study-config-file))
                 (fboundp 'json-available-p)
                 (json-available-p))
        (setq-local boa-ide-study-config-p
                    (file-attribute-modification-time (file-attributes (boa-ide--study-config-file))))
        (boa-ide-process-study-config)))
    (c-update-modeline)))

(provide 'boa-ide)

;;; boa-ide.el ends here
