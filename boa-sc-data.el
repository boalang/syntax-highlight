;;; boa-sc-data.el --- Data management for study-config data  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 1.2.1
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

(defvar boa-sc-buffers (list)
  "Alist from projects to study-config buffers.")

(defvar boa-sc-data (make-hash-table)
  "Hash table from Project to Study Config data.")

(defvar boa-sc-last-parse (make-hash-table)
  "Tracking for when study config was last parsed.")

(defvar boa-sc-verbosity (make-hash-table)
  "Tracking verbosity of projects.")


;; Parsing Data

(defun boa-sc-parse (project)
  "Parse study config data for PROJECT from BUFFER."
  (when (boa-sc-parse-needed-p project)
    (let* ((buffer (boa-sc-get-study-config-buffer project))
           (new-data (save-current-buffer
                       (with-current-buffer buffer
                         (save-mark-and-excursion
                           (save-match-data  ;Just don't mess with any mark/buffer/excursion info just in case the buffer is otherwise being visited...
                             (goto-char (point-min))
                             (json-parse-buffer :array-type 'list)))))))
      (unless (null new-data)
        (unless (= 0 (hash-table-count new-data))
          (puthash project new-data boa-sc-data)
          (puthash project (current-time) boa-sc-last-parse))))))

(defun boa-sc-parse-needed-p (project)
  "Is a parse needed for PROJECT?"
  (or (null (gethash project boa-sc-data))
      (null (gethash project boa-sc-last-parse))
      (< (time-convert (gethash project boa-sc-last-parse) 'integer) ;Times are stored as a structure that can't be easily compared (I could use `ts.el', but I don't want to add external dependencies)
         (time-convert (file-attribute-modification-time (file-attributes (boa-sc-get-study-config-file project))) 'integer))))


;; Utility Functions

(defun boa-sc-get-study-config-file (project)
  "Get the name of the study-config file for PROJECT."
  (file-name-concat project "study-config.json"))

(defun boa-sc-get-study-config-buffer (project)
  "Get the buffer for PROJECT."
  (setf boa-sc-buffers (cl-remove-if-not #'buffer-live-p boa-sc-buffers :key #'cdr)) ;Clear out dead buffers first
  (if-let ((buffer (cdr (assoc project boa-sc-buffers #'string=))))
      buffer
    (let ((buffer (save-mark-and-excursion
                    (or (find-buffer-visiting (boa-sc-get-study-config-file project))
                        (find-file-noselect (boa-sc-get-study-config-file project))))))
      (setf boa-sc-buffers (cons (cons project buffer)
                                 boa-sc-buffers))
      buffer)))

(defun boa-sc-get-data (project)
  "Get the study-config data for PROJECT."
  (when (boa-sc-parse-needed-p project)
    (boa-sc-parse project))
  (gethash project boa-sc-data))

(defun boa-sc-get-project-dir ()
  "Get the full name of the project directory."
  (let ((project-dir (locate-dominating-file              ; More general than previous `project-current' call (does not need to fit definition of "project")
                      (buffer-file-name (current-buffer)) "study-config.json")))
    (when project-dir
      (expand-file-name                     ; File name should be expanded (i.e., no shortcuts like ~) so that there aren't hash misses
       project-dir))))

(defun boa-sc-get-or-read-project (prompt)
  "PROMPT for a known boa-sc project."
  (or (boa-sc-get-project-dir)
      (completing-read prompt
                       (let ((projs (list)))
                         (maphash #'(lambda (key val)
                                      (cl-pushnew key projs :test #'string=))
                                  boa-sc-data)
                         projs)
                       nil t)))


;; Get Boa items

(defun boa-sc-datasets (project)
  "Get known datasets for PROJECT."
  (let ((outputs (list)))
    (maphash #'(lambda (key value)
                 (cl-pushnew key outputs :test #'string=))
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
  "Set verbosity to LEVEL for PROJECT.

LEVEL can be nil, t or an integer.  When nil, no verbosity is
used.  When LEVEL is 1, simply set to t.  If less than or equal
to 0, clear verbosity setting.  If greater than 5, cap to 5, in
any other case, set to provided value."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (boa-sc-get-or-read-project "Project? ")))
  (cond
   ((<= level 0)
    (remhash project boa-sc-verbosity))
   ((> level 5)
    (puthash project 5 boa-sc-verbosity))
   ((= level 1)
    (puthash project t boa-sc-verbosity))
   (t
    (puthash project level boa-sc-verbosity))))

(defun boa-sc-get-verbosity (project)
  "Get a string describing verbosity for PROJECT.

If called interactively, print as a message."
  (interactive (list (boa-sc-get-or-read-project "Project? ")))
  (let* ((verbosity-value (gethash project boa-sc-verbosity))
         (verbosity-string (cond
                            ((integerp verbosity-value)
                             (format "VERBOSE=-%s" (make-string verbosity-value ?v)))
                            ((null verbosity-value)
                             "")
                            (t
                             "VERBOSE=-v"))))
    (if (called-interactively-p 'interactive)
        (message "Compilations will be called with `%s'." verbosity-string)
      (format " %s" verbosity-string))))

(defun boa-sc-compile (project target)
  "Compile TARGET in PROJECT."
  (let ((compilation-directory project)
        (default-directory project))
    (compilation-start (format "make %s %s" (boa-sc-get-verbosity project) target) nil)))

(provide 'boa-sc-data)

;;; boa-sc-data.el ends here.
