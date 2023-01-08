;;; boa-ide.el --- Mode for boa language files  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 2.3.1
;; Package-Requires: ((boa-sc-data "1.2.3") (boa-mode "1.4.4"))
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

(require 'boa-sc-data)
(require 'cl-lib)

;;; Code:


;;; Mode variables and data storage.
(defvar-local boa-ide-project-dir nil
  "Directory of current Boa project.")

(defvar-local boa-ide-file-relative-name nil
  "Relative name of Boa file.")


;;; Running targets

(defun boa-ide-run-query (query)
  "Run the Boa query QUERY."
  (interactive (list (format "data/txt/%s" (completing-read "Query: "
                                                            (boa-sc-outputs-query boa-ide-project-dir
                                                                                  boa-ide-file-relative-name)
                                                            nil t))))
  (boa-sc-compile boa-ide-project-dir query))

(defun boa-ide-run-csv (csv)
  "Generate csv file CSV."
  (interactive (list (format "data/csv/%s" (completing-read "CSV: "
                                                            (boa-sc-csv-query boa-ide-project-dir
                                                                              boa-ide-file-relative-name)
                                                            nil t))))
  (boa-sc-compile boa-ide-project-dir csv))

(defun boa-ide-run-analysis (analysis)
  "Run the analysis ANALYSIS."
  (interactive (list (completing-read "Analysis: "
                                      (boa-sc-analyses-query boa-ide-project-dir
                                                             boa-ide-file-relative-name)
                                      nil t)))
  (boa-sc-compile boa-ide-project-dir analysis))

(defun boa-ide-run-any (target)
  "Run TARGET."
  (interactive (list (completing-read "Target: "
                                      (append
                                       (mapcar (apply-partially 'format "data/txt/%s")
                                               (boa-sc-outputs-query boa-ide-project-dir
                                                                     boa-ide-file-relative-name))
                                       (mapcar (apply-partially 'format "data/csv/%s")
                                               (boa-sc-csv-query boa-ide-project-dir
                                                                 boa-ide-file-relative-name))
                                       (boa-sc-analyses-query boa-ide-project-dir
                                                              boa-ide-file-relative-name))
                                      nil t)))
  (boa-sc-compile boa-ide-project-dir target))

(defun boa-ide-pop-to-study-config ()
  "Pop to buffer's study-config."
  (interactive)
  (pop-to-buffer (boa-sc-get-study-config-buffer boa-ide-project-dir)))


;;; Preview queries.

(defun boa-ide-prepare-file-replacement (prefix file root)
  "Prepare FILE (relative to ROOT) for use as replacement, using PREFIX.

PREFIX is checked to ensure that it is only whitespace.  If so,
it is retained, otherwise, no prefix is used."
  (let ((prefix (save-match-data
                  (if (string-match "\\S-" prefix)
                      nil
                    prefix)))
        (file-name  (expand-file-name file (expand-file-name "boa/snippets" root))))
    (save-match-data
      (with-temp-buffer
        (insert-file file-name)
        (goto-char (point-min))
        (when prefix
          (while (progn (forward-line) (not (looking-at "^$")))
            (beginning-of-line)
            (insert prefix)))
        (buffer-string)))))

(defun boa-ide-preview-query (query)
  "Produce a preview of QUERY.

This uses the same general logic to perform substitution as the
study template.  A new buffer will be created with a filename of
the form \"*Boa Query Preview for QUERY*\", filled and
substitutions made.  It is shown with `pop-to-buffer'."
  (interactive (list (completing-read "Query: "
                                      (boa-sc-outputs-query boa-ide-project-dir
                                                            boa-ide-file-relative-name)
                                      nil t)))
  (let ((orig-buffer (current-buffer))
        (buffer (generate-new-buffer (format "*Boa Query Preview for %s*" query)))
        (changedp t)
        (substitutions-list (boa-sc-snippets-for-query boa-ide-project-dir query))
        (root-dir boa-ide-project-dir))
    (with-current-buffer buffer
      (insert-buffer-substring-no-properties orig-buffer)
      (goto-char (point-min))
      (while changedp
        (setf changedp nil)
        (dolist (substitution substitutions-list)
          (save-match-data
            (cl-destructuring-bind (target type replacement) substitution
              (goto-char (point-min))
              (let ((regexp (format "\\(.*\\)\\(%s\\)" (regexp-quote target))))
                (when (re-search-forward regexp nil t)
                  (setf changedp t)
                  (message "Replacing target \"%s\"." target)
                  (let* ((prefix (match-string 1))
                         (debug type)
                         (replacement-string (if (equal type :string)
                                                 replacement
                                               (boa-ide-prepare-file-replacement prefix replacement root-dir))))
                    (replace-match replacement-string nil t nil 2))))))))
      (boa-mode)
      (read-only-mode))
    (pop-to-buffer buffer)))


;;; Completion

(defun boa-ide-complete-snippets ()
  "Offer snippet completions."
  (let* ((symbol-bounds (bounds-of-thing-at-point 'symbol))
         (symbol-start (car symbol-bounds))
         (symbol-end (cdr symbol-bounds))
         (line-bounds (bounds-of-thing-at-point 'line))
         (line-start (car line-bounds))
         (line-end (cdr line-bounds))
         (snippets (mapcar (lambda (str)
                             (substring str 2 (- (length str) 2)))
                           (boa-sc-snippets boa-ide-project-dir))))
    (when-let ((new-start (save-excursion
                            (save-match-data
                              (search-backward "{@" line-start t))))
               (new-end (save-excursion
                          (save-match-data
                            (search-forward "@}" line-end t)))))
      (list (+ new-start 2) (- new-end 2) snippets))))


;;; Mode definition

(defvar boa-ide-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (binding)
              (cl-destructuring-bind (binding function) binding
                (define-key map (kbd binding) function)))
          '(("C-c C-r q" boa-ide-run-query)
            ("C-c C-r c" boa-ide-run-csv)
            ("C-c C-r a" boa-ide-run-analysis)
            ("C-c C-r C" boa-ide-pop-to-study-config)
            ("C-c C-r P" boa-ide-preview-query)
            ("C-c C-c" boa-ide-run-any)))
    map)
  "Keymap for Boa IDE Support.")

(define-minor-mode boa-ide-mode
  "Provides support for Boa study template projects."
  :lighter nil
  :interactive (list 'boa-mode)
  :keymap boa-ide-mode-map
  (when (and boa-ide-mode
             (boa-sc-get-project-dir))
    (setq-local boa-ide-project-dir (boa-sc-get-project-dir))
    (setq-local boa-ide-file-relative-name
                (file-relative-name (buffer-file-name)
                                    (expand-file-name "boa" boa-ide-project-dir)))
    (setq-local completion-at-point-functions (cons 'boa-ide-complete-snippets completion-at-point-functions))
    (c-update-modeline)))

(provide 'boa-ide)

;;; boa-ide.el ends here
