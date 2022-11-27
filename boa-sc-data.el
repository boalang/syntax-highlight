;;; boa-sc-data.el --- Data management for study-config data

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.0.0
;; Package-Requires: (cl-lib)
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
;; This file allows managing data for Boa studies.

(require 'cl-lib)

;;; Code:


;; Variables

(defvar boa-sc-data (make-hash-table)
  "Hash table from Project to Study Config data.")

(defvar boa-sc-last-parse (make-hash-table)
  "Tracking for when study config was last parsed.")

(defvar boa-sc-verbosity (make-hash-table)
  "Tracking verbosity of projects.")


;; Parsing Data

(defun boa-sc-parse (project buffer)
  "Parse study config data for PROJECT from BUFFER."
  (when (boa-sc-parse-needed-p project)
    (let ((new-data (save-current-buffer
                      (with-current-buffer buffer
                        (save-mark-and-excursion
                          (goto-char (point-min))
                          (json-parse-buffer :array-type 'list))))))
      (unless (null new-data)
        (unless (= 0 (hash-table-count new-data))
          (puthash project new-data boa-sc-data)
          (puthash project (current-time) boa-sc-last-parse))))))

(defun boa-sc-parse-needed-p (project)
  "Is a parse needed for PROJECT?"
  (or (null (gethash project boa-sc-data))
      (null (gethash project boa-sc-last-parse))
      (< (time-convert (gethash project boa-sc-last-parse) 'integer)
         (time-convert (file-attribute-modification-time (file-attributes (boa-sc-get-study-config-file project))) 'integer))))


;; Utility Functions

(defun boa-sc-get-study-config-file (project)
  "Get the name of the study-config file for PROJECT."
  (file-name-concat project "study-config.json"))

(defun boa-sc-get-study-config-buffer (project)
  "Get the buffer for PROJECT."
  (save-mark-and-excursion
    (or (find-buffer-visiting project)
        (find-file-noselect (boa-sc-get-study-config-file project)))))

(defun boa-sc-get-data (project)
  "Get the study-config data for PROJECT."
  (when (boa-sc-parse-needed-p project)
    (boa-sc-parse project (boa-sc-get-study-config-buffer project)))
  (gethash project boa-sc-data))

(defun boa-sc-get-project-dir ()
  "Get the full name of the project directory."
  (expand-file-name (cdr (project-current))))


;; Get Boa items

(defun boa-sc-datasets (project)
  "Get known datasets for PROJECT."
  (let ((outputs (list)))
    (maphash #'(lambda (key value)
                 (cl-pushnew key outputs :test #'string=)
                 (when-let ((processors (gethash "processors" value)))
                   (maphash #'(lambda (key processor)
                                (when-let ((output (gethash "output" processor)))
                                  (cl-pushnew (substring output 9) outputs :test #'string=)))
                            processors)))
             (gethash "datasets" (boa-sc-get-data project)))
    outputs))

(defun boa-sc-outputs (project)
  "Get known outputs for PROJECT."
  (let ((outputs (list)))
    (maphash #'(lambda (key value)
                 (cl-pushnew key outputs :test #'string=)
                 (when-let ((processors (gethash "processors" value)))
                   (maphash #'(lambda (key processor)
                                (when-let ((output (gethash "output" processor)))
                                  (cl-pushnew (substring output 9) outputs :test #'string=)))
                            processors)))
             (gethash "queries" (boa-sc-get-data project)))
    outputs))

(defun boa-sc-csvs (project)
  "Get known CSV outputs for PROJECT."
  (let ((csvs (list)))
    (maphash #'(lambda (key output)
                 (when-let ((processors (gethash "processors" output)))
                   (maphash #'(lambda (key processor)
                                (when-let ((csv (gethash "csv" processor)))
                                  (if (stringp csv)
                                      (cl-pushnew csv csvs :test #'string=)
                                    (cl-pushnew (gethash "output" csv) csvs :test #'string=))))
                            processors))
                 (when-let ((csv (gethash "csv" output)))
                   (if (stringp csv)
                       (cl-pushnew csv csvs :test #'string=)
                     (cl-pushnew (gethash "output" csv) csvs :test #'string=))))
             (gethash "queries" (boa-sc-get-data project)))
    csvs))

(defun boa-sc-analyses (project)
  "List analyses for PROJECT."
  (let ((analyses (list)))
    (maphash #'(lambda (key value)
                 (cl-pushnew key analyses :test #'string=))
             (gethash "analyses" (boa-sc-get-data project)))
    analyses))

(defun boa-sc-snippets (project)
  "List known snippets for PROJECT."
  (let ((substitutions (list)))
    (mapc #'(lambda (substitution)
              (cl-pushnew (gethash "target" substitution) substitutions :test #'string=))
          (gethash "substitutions" (boa-sc-get-data project)))
    substitutions))


;; Find items from queries

(defun boa-sc-outputs-query (project query)
  "Get outputs for QUERY in PROJECT."
  (let ((outputs (list)))
    (maphash #'(lambda (key value)
                 (when (string= query (gethash "query" value))
                   (cl-pushnew key outputs :test #'string=)
                   (when-let ((processors (gethash "processors" value)))
                     (maphash #'(lambda (key processor)
                                  (cl-pushnew (substring (gethash "output" processor) 8) outputs :test #'string=))
                              processors))))
             (gethash "queries" (boa-sc-get-data project)))
    outputs))

(defun boa-sc-csv-query (project query)
  "Get CSV files for QUERY in PROJECT."
  (let ((csvs (list)))
    (maphash #'(lambda (key value)
                 (when (string= query (gethash "query" value))
                   (when-let ((csv (gethash "csv" value)))
                     (if (stringp csv)
                         (cl-pushnew csv csvs :test #'string=)
                       (cl-pushnew (gethash "output" csv) csvs :test #'string=)))
                   (when-let ((processors (gethash "processors" value)))
                     (maphash #'(lambda (key processor)
                                  (when-let ((csv (gethash "csv" processor)))
                                    (if (stringp csv)
                                        (cl-pushnew csv csvs :test #'string=)
                                      (cl-pushnew (gethash "output" csv) csvs :test #'string=))))
                              processors))))
             (gethash "queries" (boa-sc-get-data project)))
    csvs))

(defun boa-sc-analyses-query (project query)
  "Get Analyses for QUERY in PROJECT."
  (let ((analyses (list))
        (outputs-csvs (append (boa-sc-csv-query project query)
                              (boa-sc-outputs-query project query))))
    (maphash #'(lambda (key value)
                 (when-let ((inputs (gethash "input" value)))
                   (when (cl-intersection outputs-csvs inputs :test #'string=)
                     (cl-pushnew (file-name-sans-extension key) analyses :test #'string=))))
             (gethash "analyses" (boa-sc-get-data project)))
    analyses))



;; Compilation Functions
(defun boa-sc-set-verbose (level project)
  "Set verbosity to LEVEL for PROJECT."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (boa-sc-get-project-dir)
                         (completing-read "Project? "
                                          (let ((projs (list)))
                                            (maphash #'(lambda (key val)
                                                         (cl-pushnew key projs :test #'string=))
                                                     boa-sc-data)
                                            projs)
                                          nil t))))
  (cond
   ((< 0 level)
    (remhash project boa-sc-verbosity))
   ((> level 5)
    (puthash project 5 boa-sc-verbosity))
   ((= level 1)
    (puthash project t boa-sc-verbosity))
   (t
    (puthash project level boa-sc-verbosity))))

(defun boa-sc-get-verbosity (project)
  "Get a string describing verbosity for PROJECT."
  (let ((verbosity-value (gethash project boa-sc-verbosity)))
    (cond
     ((integerp verbosity-value)
      (format " VERBOSE=-%s" (make-string verbosity-value ?v)))
     ((null verbosity-value)
      "")
     (t
      " VERBOSE=-v"))))

(defun boa-sc-compile (project target)
  "Compile TARGET in PROJECT."
  (let ((compilation-directory project)
        (default-directory project))
    (compilation-start (format "make %s %s" (boa-sc-get-verbosity project) target) nil)))

(provide 'boa-sc-data)

;;; boa-sc-data.el ends here.
