;;; boa-ide-sc.el --- Data management for study-config data  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 2.0.1
;; Keywords: languages
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


;; Structures

(cl-defstruct boa-ide-sc-configuration
  "Overall configuration of a study-config.json project.

Slots are:

 - `project'
 - `buffer'
 - `last-parse'
 - `datasets'
 - `queries'
 - `substitutions'
 - `analyses'"
  project
  buffer
  last-parse
  datasets
  queries
  substitutions
  analyses)

(cl-defstruct boa-ide-sc-query
  "Structure describing a Boa query."
  output-file
  query-file
  dataset
  substitutions
  public-p
  csv-output
  processors)

(cl-defstruct boa-ide-sc-processor
  "Structure describing a post-processor."
  script-file
  output-file
  csv-output
  cache-clean)

(cl-defstruct boa-ide-sc-analysis
  "Structure describing an analysis."
  script-file
  input-files
  disabled-p)

(cl-defstruct boa-ide-sc-substitution
  "Structure describing a substitution."
  target
  type
  replacement)

(cl-defstruct boa-ide-sc-csv
  "Structure describing CSV generation."
  output-file
  test
  drop
  header
  index-columns)


;; Variables

(defvar boa-ide-sc-data (make-hash-table :test 'equal)
  "Hash table from Project to Study Config data.")

(defvar boa-ide-sc-verbosity (make-hash-table :test 'equal)
  "Tracking verbosity of projects.")


;; Conversion Functions

(defun boa-ide-sc-maphash (function table)
  "Robust `maphash' of FUNCTION over TABLE."
  (when-let ((table table))
    (maphash function table)))

(defun boa-ide-sc-convert-analysis (name content)
  "Convert the analysis map CONTENT with NAME to a `boa-ide-sc-analysis'."
  (make-boa-ide-sc-analysis :script-file name
                            :input-files (gethash "input" content)
                            :disabled-p (gethash "disabled" content)))

(defun boa-ide-sc-convert-substitution (map)
  "Convert the substitution MAP to a `boa-ide-sc-substitution'."
  (make-boa-ide-sc-substitution :target (gethash "target" map)
                                :type (if (gethash "file" map) :file :string)
                                :replacement (if (gethash "file" map)
                                                 (gethash "file" map)
                                               (gethash "replacement" map))))

(defun boa-ide-sc-convert-csv (object)
  "Convert OBJECT to a `boa-ide-sc-csv'."
  (if (stringp object)
      (make-boa-ide-sc-csv :output-file object)
    (make-boa-ide-sc-csv :output-file (gethash "output" object)
                         :test (gethash "test" object)
                         :drop (gethash "drop" object)
                         :header (gethash "header" object)
                         :index-columns (gethash "index" object))))

(defun boa-ide-sc-convert-processor (name map)
  "Convert MAP with NAME to a `boa-ide-sc-processor'."
  (if (stringp map)
      (make-boa-ide-sc-processor :script-file name
                                 :output-file map)
    (make-boa-ide-sc-processor :script-file name
                               :output-file (gethash "output" map)
                               :csv-output (when-let ((csv-object (gethash "csv" map)))
                                             (boa-ide-sc-convert-csv csv-object))
                               :cache-clean (gethash "cacheclean" map))))

(defun boa-ide-sc-convert-query (name map)
  "Convert the query MAP with NAME to a `boa-ide-sc-query'."
  (make-boa-ide-sc-query :output-file name
                         :query-file (gethash "query" map)
                         :dataset (gethash "dataset" map)
                         :substitutions (mapcar #'boa-ide-sc-convert-substitution (gethash "substitutions" map))
                         :public-p (gethash "public" map)
                         :csv-output (when-let ((csv-object (gethash "csv" map)))
                                       (boa-ide-sc-convert-csv csv-object))
                         :processors (let ((processors (make-hash-table :test 'equal)))
                                       (boa-ide-sc-maphash (lambda (name content)
                                                             (puthash name (boa-ide-sc-convert-processor name content) processors))
                                                           (gethash "processors" map))
                                       processors)))

(defun boa-ide-sc-convert-configuration (project buffer map)
  "Build a `boa-ide-sc-configuration' from MAP for PROJECT in BUFFER."
  (make-boa-ide-sc-configuration :project project
                                 :buffer buffer
                                 :datasets (gethash "datasets" map)
                                 :last-parse (current-time)
                                 :analyses (let ((analyses (make-hash-table :test 'equal)))
                                             (boa-ide-sc-maphash (lambda (name content)
                                                                   (puthash name (boa-ide-sc-convert-analysis name content) analyses))
                                                                 (gethash "analyses" map))
                                             analyses)
                                 :queries (let ((queries (make-hash-table :test 'equal)))
                                            (boa-ide-sc-maphash (lambda (name content)
                                                                  (puthash name (boa-ide-sc-convert-query name content) queries))
                                                                (gethash "queries" map))
                                            queries)
                                 :substitutions (mapcar #'boa-ide-sc-convert-substitution (gethash "substitutions" map))))


;; Parsing Data

(defun boa-ide-sc-parse (project)
  "Parse study config data for PROJECT."
  (let ((buffer (boa-ide-sc-get-study-config-buffer project)))
    (save-current-buffer
      (with-current-buffer buffer
        (save-mark-and-excursion
          (save-match-data
            (if (buffer-modified-p)
                (message "Buffer %s is modified.  Unwise to parse JSON in modified buffer." (buffer-name))
              (condition-case error-data
                  (let ((new-data
                         (progn
                           (message "Parsing %s." (buffer-name))
                           (goto-char (point-min))
                           (json-parse-buffer :array-type 'list))))
                    (unless (or (null new-data)
                                (= 0 (hash-table-count new-data)))
                      (puthash project (boa-ide-sc-convert-configuration project buffer new-data) boa-ide-sc-data)))
                (json-parse-error
                 (display-warning 'boa-ide-sc-data
                                  (cl-destructuring-bind (_error-name message _loc line column abs-loc) error-data
                                    (format "A JSON parsing error has occured (\"%s\") in %s."
                                            message
                                            (propertize (format "%s:%d:%d" (buffer-name) line column)
                                                        'font-lock-face 'link
                                                        'keymap `(keymap (mouse-1 . (lambda ()
                                                                                      (interactive)
                                                                                      (pop-to-buffer ,(current-buffer))
                                                                                      (goto-char ,abs-loc)))))))
                                  :error))))))))))

(defun boa-ide-sc-parse-needed-p (project)
  "Is a parse needed for PROJECT?"
  (or (null (gethash project boa-ide-sc-data))
      (< (time-convert (boa-ide-sc-configuration-last-parse (gethash project boa-ide-sc-data)) 'integer) ;Still not going to use `ts.el'...
         (time-convert (file-attribute-modification-time (file-attributes (boa-ide-sc-get-study-config-file project))) 'integer))))


;; Utility Functions

(defun boa-ide-sc-get-study-config-file (project)
  "Get the name of the study-config file for PROJECT."
  (file-name-concat project "study-config.json"))

(defun boa-ide-sc-get-study-config-buffer (project)
  "Get the buffer for PROJECT."
  (if-let ((configuration (gethash project boa-ide-sc-data))
           (buffer (and (buffer-live-p (boa-ide-sc-configuration-buffer configuration))
                        (boa-ide-sc-configuration-buffer configuration))))
      buffer
    (save-mark-and-excursion
      (or (find-buffer-visiting (boa-ide-sc-get-study-config-file project))
          (find-file-noselect (boa-ide-sc-get-study-config-file project))))))

(defun boa-ide-sc-get-data (project)
  "Get the study-config data for PROJECT."
  (when (boa-ide-sc-parse-needed-p project)
    (boa-ide-sc-parse project))
  (gethash project boa-ide-sc-data))

(defun boa-ide-sc-get-project-dir ()
  "Get the full name of the project directory."
  (when (buffer-file-name (current-buffer))
    (let ((project-dir (locate-dominating-file              ; More general than previous `project-current' call (does not need to fit definition of "project")
                        (buffer-file-name (current-buffer)) "study-config.json")))
      (when project-dir
        (expand-file-name                     ; File name should be expanded (i.e., no shortcuts like ~) so that there aren't hash misses
         project-dir)))))

(defun boa-ide-sc-get-or-read-project (prompt)
  "PROMPT for a known boa-ide-sc project."
  (or (boa-ide-sc-get-project-dir)
      (completing-read prompt
                       (let ((projs (list)))
                         (maphash (lambda (key _)
                                    (cl-pushnew key projs :test #'string=))
                                  boa-ide-sc-data)
                         projs)
                       nil t)))


;; Get targets, datasets, substitutions (return list of strings)

(defun boa-ide-sc-datasets (project)
  "Get known datasets for PROJECT."
  (let ((datasets (list)))
    (boa-ide-sc-maphash (lambda (key _)
                          (cl-pushnew key datasets :test #'string=))
                        (boa-ide-sc-configuration-datasets (boa-ide-sc-get-data project)))
    datasets))

(defun boa-ide-sc-outputs (project)
  "Get known outputs for PROJECT."
  (let ((outputs (list)))
    (boa-ide-sc-maphash (lambda (output query)
                          (cl-pushnew output outputs :test #'string=)
                          (boa-ide-sc-maphash (lambda (_ processor)
                                                (when-let ((output (boa-ide-sc-processor-output-file processor)))
                                                  (cl-pushnew output outputs :test #'string=)))
                                              (boa-ide-sc-query-processors query)))
                        (boa-ide-sc-configuration-queries (boa-ide-sc-get-data project)))
    outputs))

(defun boa-ide-sc-csvs (project)
  "Get known CSV outputs for PROJECT."
  (let ((csvs (list)))
    (boa-ide-sc-maphash (lambda (_ query)
                          (when-let ((csv (boa-ide-sc-query-csv-output query)))
                            (cl-pushnew (boa-ide-sc-csv-output-file csv) csvs :test #'string=))
                          (boa-ide-sc-maphash (lambda (_ processor)
                                                (when-let ((csv (boa-ide-sc-processor-csv-output processor)))
                                                  (cl-pushnew (boa-ide-sc-csv-output-file csv) csvs :test #'string=)))
                                              (boa-ide-sc-query-processors query)))
                        (boa-ide-sc-configuration-queries (boa-ide-sc-get-data project)))
    csvs))

(defun boa-ide-sc-analyses (project)
  "Get known analyses for PROJECT."
  (let ((analyses (list)))
    (boa-ide-sc-maphash (lambda (analysis _)
                          (cl-pushnew (file-name-sans-extension analysis) analyses :test #'string=))
                        (boa-ide-sc-configuration-analyses (boa-ide-sc-get-data project)))
    analyses))

(defun boa-ide-sc-snippets (project)
  "Get known snippets for PROJECT."
  (mapcar #'boa-ide-sc-substitution-target
          (boa-ide-sc-configuration-substitutions (boa-ide-sc-get-data project))))


;; Find items from queries (return list of strings)

(defun boa-ide-sc-outputs-query (project query)
  "Get the outputs for QUERY in PROJECT."
  (let ((outputs (list)))
    (boa-ide-sc-maphash (lambda (output object)
                          (when (string= query (boa-ide-sc-query-query-file object))
                            (cl-pushnew output outputs :test #'string=)
                            (boa-ide-sc-maphash (lambda (_ obj)
                                                  (when-let ((output (boa-ide-sc-processor-output-file obj)))
                                                    (cl-pushnew output outputs :test #'string=)))
                                                (boa-ide-sc-query-processors object))))
                        (boa-ide-sc-configuration-queries (boa-ide-sc-get-data project)))
    outputs))

(defun boa-ide-sc-csv-query (project query)
  "Get the CSV outputs for QUERY in PROJECT."
  (let ((csvs (list)))
    (boa-ide-sc-maphash (lambda (_ object)
                          (when (string= query (boa-ide-sc-query-query-file object))
                            (when-let ((csv (boa-ide-sc-query-csv-output object)))
                              (cl-pushnew (boa-ide-sc-csv-output-file csv) csvs :test #'string=))
                            (boa-ide-sc-maphash (lambda (_ obj)
                                                  (when-let ((csv (boa-ide-sc-processor-csv-output obj)))
                                                    (cl-pushnew csv csvs :test #'string=)))
                                                (boa-ide-sc-query-processors object))))
                        (boa-ide-sc-configuration-queries (boa-ide-sc-get-data project)))
    csvs))

(defun boa-ide-sc-analyses-query (project query)
  "Get the analyses for QUERY in PROJECT."
  (let ((analyses (list))
        (outputs-csvs (append (boa-ide-sc-csv-query project query)
                              (boa-ide-sc-outputs-query project query))))
    (boa-ide-sc-maphash (lambda (analysis object)
                          (when-let* ((inputs (boa-ide-sc-analysis-input-files object))
                                      (intersection (cl-intersection inputs outputs-csvs :test #'string=)))
                            (cl-pushnew (file-name-sans-extension analysis) analyses :test #'string=)))
                        (boa-ide-sc-configuration-analyses (boa-ide-sc-get-data project)))
    analyses))


;; Get a list of snippets for query (as objects)

(defun boa-ide-sc-snippets-query (project query)
  "Get the snippets for QUERY in PROJECT."
  (when-let* ((project (boa-ide-sc-get-data project))
              (query (gethash query (boa-ide-sc-configuration-queries project))))
    (let ((snippets (append (reverse (boa-ide-sc-query-substitutions query))
                            (reverse (boa-ide-sc-configuration-substitutions project))))
          (final-snippets (list)))
      (mapcan (lambda (snippet)
                (cl-pushnew snippet final-snippets :test #'string= :key #'boa-ide-sc-substitution-target))
              snippets)
      final-snippets)))


;; Compilation Functions
(defun boa-ide-sc-set-verbose (level project)
  "Set verbosity to LEVEL for PROJECT.

LEVEL can be nil, t or an integer.  When nil, no verbosity is
used.  When LEVEL is 1, simply set to t.  If less than or equal
to 0, clear verbosity setting.  If greater than 5, cap to 5, in
any other case, set to provided value."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (boa-ide-sc-get-or-read-project "Project? ")))
  (cond
   ((<= level 0)
    (remhash project boa-ide-sc-verbosity))
   ((> level 5)
    (puthash project 5 boa-ide-sc-verbosity))
   ((= level 1)
    (puthash project t boa-ide-sc-verbosity))
   (t
    (puthash project level boa-ide-sc-verbosity))))

(defun boa-ide-sc-get-verbosity (project)
  "Get a string describing verbosity for PROJECT.

If called interactively, print as a message."
  (interactive (list (boa-ide-sc-get-or-read-project "Project? ")))
  (let* ((verbosity-value (gethash project boa-ide-sc-verbosity))
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

(defun boa-ide-sc-compile (project target)
  "Compile TARGET in PROJECT."
  (let ((default-directory project))
    (compilation-start (format "make%s %s" (boa-ide-sc-get-verbosity project) target) nil)))

(provide 'boa-ide-sc)

;;; boa-ide-sc.el ends here.
