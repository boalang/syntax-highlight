;;; boa-ide.el --- Mode for boa language files

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 0.0.1
;; Package-Requires: ((json-snatcher "1.0") (json-mode "1.6.0") (project "0.8.1"))
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

(require 'project)
(require 'cl-lib)
(require 'json-snatcher)
(require 'json-mode)

;;; Code:


;;; Mode variables


;;; Data Model

(defvar-local boa-sc-study-config-data nil
  "Contents of Boa Study Config, parsed.")

(defvar-local boa-sc-study-config-last-parse nil
  "When was the Study Config last parsed?")

(defun boa-sc--parse-config ()
  "Parse and store Boa Study Config data."
  (when (or (null boa-sc-study-config-data)
            (null boa-sc-study-config-last-parse)
            (buffer-modified-p)
            (< (time-convert boa-sc-study-config-last-parse 'integer)
               (time-convert (file-attribute-modification-time (file-attributes (buffer-file-name))) 'integer)))
    (let ((new-data (save-excursion
                      (ignore-errors
                        (json-parse-buffer :array-type 'list)))))
      (unless (null new-data)
        (unless (hash-table-empty-p new-data)
          (setq-local boa-sc-study-config-last-parse (current-time)
                      boa-sc-study-config-data new-data))
        ))
    (message "Parsed study config.")))

(defun boa-sc--datasets ()
  "Collect a list of known datasets."
  (boa-sc--parse-config)
  (let ((datasets (list)))
    (maphash #'(lambda (key value)
                 (cl-pushnew key datasets :test #'string=))
             (gethash "datasets" boa-sc-study-config-data))
    datasets))

(defun boa-sc--outputs ()
  "Collect a list of known outputs."
  (boa-sc--parse-config)
  (let ((outputs (list)))
    (maphash #'(lambda (key value)
                 (cl-pushnew key outputs :test #'string=)
                 (when-let ((processors (gethash "processors" value)))
                   (maphash #'(lambda (key processor)
                                (when-let ((output (gethash "output" processor)))
                                  (cl-pushnew (substring output 9) outputs :test #'string=)))
                            processors)))
             (gethash "queries" boa-sc-study-config-data))
    outputs))

(defun boa-sc--csvs ()
  "Collect a list of known CSV outputs."
  (boa-sc--parse-config)
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
             (gethash "queries" boa-sc-study-config-data))
    csvs))

(defun boa-sc--analyses ()
  "Collect a list of known analyses."
  (boa-sc--parse-config)
  (let ((analyses (list)))
    (maphash #'(lambda (key value)
                 (cl-pushnew key analyses :test #'string=))
             (gethash "analyses" boa-sc-study-config-data))
    analyses))


;;; Current Context

(defun boa-sc-current-context ()
  "Determine current context."
  (when-let ((path (jsons-get-path)))
    (cond
     ((and (stringp (nth 0 path))
           (string= (nth 0 path) "\"query\""))
      :query-fn)
     ((and (string= (car (last path)) "\"queries\"")
           (= (length path) 2))
      :output-fn)
     ((and (stringp (nth 0 path))
           (string= (nth 0 path) "\"file\"")
           (stringp (nth (1- (length path)) path))
           (string= (nth (1- (length path)) path) "\"substitutions\""))
      :substitution-fn)
     ((and (stringp (nth 1 path))
           (string= (nth 1 path) "\"input\""))
      :inputs-list)
     ((and (stringp (nth 0 path))
           (string= (nth 0 path) "\"dataset\"")
           (setf completions (append (boa-sc--datasets) completions)))
      :dataset-name)
     ((and (cl-member "\"processors\"" (cl-remove-if #'numberp path) :test #'string=)
           (stringp (nth 0 path))
           (string= (nth 0 path) "\"output\""))
      :processor-output)
     ((and (stringp (nth 0 path))
           (string= (nth 0 path) "\"csv\""))
      :csv-as-fn)
     ((and (cl-member "\"csv\"" (cl-remove-if #'numberp path) :test #'string=)
           (stringp (nth 0 path))
           (string= (nth 0 path) "\"output\""))
      :csv-output)
     ((cl-member "\"analyses\"" (cl-remove-if #'numberp path) :test #'string=)
      :analysis-def))))


;;; Completion Commands

(defun boa-sc-complete-dataset (prompt require-match initial-input)
  "Complete a dataset using PROMPT, with REQUIRE-MATCH and INITIAL-INPUT."
  (completing-read prompt (boa-sc--datasets)
                   nil require-match initial-input))

(defun boa-sc-complete-output (prompt require-match initial-input)
  "Complete an output using PROMPT, with REQUIRE-MATCH and INITIAL-INPUT."
  (completing-read prompt (mapcar #'(lambda (x) (format "data/txt/%s" x)) (boa-sc--outputs))
                   nil require-match initial-input))

(defun boa-sc-complete-csv (prompt require-match initial-input)
  "Complete a CSV using PROMPT, with REQUIRE-MATCH and INITIAL-INPUT."
  (completing-read prompt (mapcar #'(lambda (x) (format "data/csv/%s" x)) (boa-sc--csvs))
                   nil require-match initial-input)  )

(defun boa-sc-complete-csv-output (prompt require-match initial-input)
  "Complete either a CSV or Output using PROMPT, with REQUIRE-MATCH and INITIAL-INPUT."
  (completing-read prompt (append (mapcar #'(lambda (x) (format "data/txt/%s" x)) (boa-sc--outputs))
                                  (mapcar #'(lambda (x) (format "data/csv/%s" x)) (boa-sc--csvs)))
                   nil require-match initial-input))

(defun boa-sc-complete-analysis (prompt require-match initial-input)
  "Complete an analysis using PROMPT, with REQUIRE-MATCH and INITIAL-INPUT."
  (completing-read prompt (mapcar #'file-name-sans-extension (boa-sc--analyses))
                   nil require-match initial-input))

(defun boa-sc-complete-runnable (prompt require-match initial-input)
  "Complete a runnable using PROMPT, with REQUIRE-MATCH and INITIAL-INPUT."
  (completing-read prompt (append (mapcar #'(lambda (x) (format "data/txt/%s" x)) (boa-sc--outputs))
                                  (mapcar #'(lambda (x) (format "data/csv/%s" x)) (boa-sc--csvs))
                                  (mapcar #'file-name-sans-extension (boa-sc--analyses)))
                   nil require-match initial-input))

(defun boa-sc-completion-at-point ()
  "Offer relevant completions."
  (let* ((completions (pcase (boa-sc-current-context)
                        (:query-fn
                         (mapcar #'(lambda (x) (substring x 4))
                                 (directory-files-recursively "boa/queries" ".*\\.boa$")))
                        (:substitution-fn
                         (mapcar #'(lambda (x) (substring x 13))
                                 (directory-files-recursively "boa/snippets" ".*\\.boa$")))
                        (:inputs-list
                         (append (boa-sc--outputs)
                                 (boa-sc--csvs)))
                        (:dataset-name
                         (boa-sc--datasets) )))
         (bounds (bounds-of-thing-at-point 'filename))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point))))
    (unless (null completions)
      (list start
            end
            completions))))

(defun boa-sc-runnable-at-point ()
  "Determine if there is a runnable at point, and return it."
  (interactive)
  (pcase (boa-sc-current-context)
    ((or :processor-output
         :csv-as-fn
         :csv-output
         :inputs-list
         :output-fn)
     (thing-at-point 'filename))
    (:analysis-def
     (let ((file-name (car (last (butlast (jsons-get-path))))))
       (file-name-sans-extension (substring file-name 1 (1- (length file-name))))))))


;;; Build Commands

(defvar-local boa-sc-verbose nil
  "Should Boa queries be run with verbosity?")

(defun boa-sc-set-verbose (level)
  "Set the level of verbosity for running from the Study Config."
  (interactive "NVerbosity (0-5)? \n")
  (cond
   ((= level 0)
    (setq-local boa-sc-verbose nil))
   ((> level 5)
    (setq-local boa-sc-verbose 5))
   ((= level 1)
    (setq-local boa-sc-verbose t))
   (t
    (setq-local boa-sc-verbose level)))
  (message "Verbosity set to %s." boa-sc-verbose))

(defun boa-sc-compile (target)
  "Run TARGET from the Boa Study Config."
  (let ((verboseness (cond)))
    (let ((compilation-directory default-directory))
      (compilation-start (format "make %s %s" verboseness target) nil))))

(defun boa-sc-run (target)
  "Select and run TARGET from Study Config.  Use `boa-sc-runnable-at-point' to determine default input."
  (interactive (boa-sc-complete-runnable "Study Config Target? " t (boa-sc-runnable-at-point)))
  (boa-sc-compile target))


;;; Commands

(defun boa-sc-ffap ()
  "Open the file at point."
  (interactive)
  (when-let ((fap (thing-at-point 'filename t)))
    (let ((filename
           (pcase (boa-sc-current-context)
             (:query-fn
              (format "boa/%s" fap))
             (:substitution-fn
              (format "boa/snippets/%s" fap))
             (:inputs-list
              (format (if (string-suffix-p "csv" fap) "data/csv/%s" "data/txt/%s") fap))
             (:processor-output fap)
             ((or :csv-as-fn :csv-output) (format "data/csv/%s" fap))
             (:output-fn (format "data/txt/%s" fap))
             (:analysis-def (format "analyses/%s" fap)))))
      (find-file filename))))


;;; Mode definition

(setf boa-study-config-mode-map (let ((map (make-sparse-keymap)))
                                  (mapc #'(lambda (binding)
                                            (cl-destructuring-bind (binding function) binding
                                              (define-key map (kbd binding) function)))
                                        '(("C-c C-s v" boa-sc-set-verbose)
                                          ("C-c C-s f" boa-sc-ffap)
                                          ("C-c C-c" boa-sc-run)))
                                  map))

(defvar boa-study-config-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (binding)
              (cl-destructuring-bind (binding function) binding
                (define-key map (kbd binding) function)))
          '(("C-c C-s v" boa-sc-set-verbose)
            ("C-c C-s f" boa-sc-ffap)
            ("C-c C-c" boa-sc-run)))
    map)
  "Keymap for editing of Study Config.")

(defun boa-study-config-maybe-enable ()
  "Conditionally enable `boa-study-config-mode'."
  (when (and (derived-mode-p 'json-mode)
             (string-match-p "study-config.json" (buffer-file-name)))
    (message "Enabling Boa Study Config Mode.")
    (boa-study-config-mode)))

(defun boa-study-config-insinuate ()
  "Add `boa-study-config-maybe-enable' to `json-mode-hook'."
  (add-hook 'json-mode-hook #'boa-study-config-maybe-enable))

(define-minor-mode boa-study-config-mode
  "Provide support for editing Boa study-config.json files."
  :lighter " Boa/SC"
  :interactive t
  :keymap boa-study-config-mode-map
  (when boa-study-config-mode
    (message "Boa Study Config...")
    (boa-sc--parse-config)
    (add-hook 'after-save-hook #'boa-sc--parse-config -100 t)
    (setq-local completion-at-point-functions (cons 'boa-sc-completion-at-point completion-at-point-functions))))

(provide 'boa-study-config)

;;; boa-study-config.el ends here
