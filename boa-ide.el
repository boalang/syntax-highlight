;;; boa-ide.el --- Mode for boa language files  -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Version: 3.0.0
;; Package-Requires: ((boa-mode "1.4.4") (emacs "28.1"))
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

;; This package provides support for the Boa Study Template
;; (https://github.com/boalang/study-template).

(require 'boa-sc)
(require 'boa-mode)
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
  (interactive (list
                (format "data/txt/%s"
                        (completing-read "Query: "
                                         (boa-sc-outputs-query boa-ide-project-dir
                                                               boa-ide-file-relative-name)
                                         nil t))))
  (boa-sc-compile boa-ide-project-dir query))

(defun boa-ide-run-csv (csv)
  "Generate csv file CSV."
  (interactive (list
                (format "data/csv/%s"
                        (completing-read "CSV: "
                                         (boa-sc-csv-query boa-ide-project-dir
                                                           boa-ide-file-relative-name)
                                         nil t))))
  (boa-sc-compile boa-ide-project-dir csv))

(defun boa-ide-run-analysis (analysis)
  "Run the analysis ANALYSIS."
  (interactive (list
                (completing-read "Analysis: "
                                 (boa-sc-analyses-query boa-ide-project-dir
                                                        boa-ide-file-relative-name)
                                 nil t)))
  (boa-sc-compile boa-ide-project-dir analysis))

(defun boa-ide-run-any (target)
  "Run TARGET."
  (interactive (list
                (completing-read "Target: "
                                 (append
                                  (mapcar (apply-partially #'format "data/txt/%s")
                                          (boa-sc-outputs-query boa-ide-project-dir
                                                                boa-ide-file-relative-name))
                                  (mapcar (apply-partially #'format "data/csv/%s")
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

(defun boa-ide-prepare-replacement (prefix root replacement)
  "Prepare REPLACEMENT given PREFIX and ROOT.

PREFIX is checked to ensure that it is only whitespace.  If so,
it is retained, otherwise, no prefix is used.

If REPLACEMENT refers to a file, the file is opened relative to
ROOT, and used."
  (let ((prefix (save-match-data
                  (unless (or (string-empty-p prefix)
                              (string-match (rx (not (syntax whitespace))) prefix))
                    prefix))))
    (save-match-data
      (with-temp-buffer
        (if (equal :string (boa-sc-substitution-type replacement))
            (insert (boa-sc-substitution-replacement replacement))
          (insert-file-contents (expand-file-name (boa-sc-substitution-replacement replacement) (expand-file-name "boa/snippets" root))))
        (goto-char (point-min))
        (when prefix
          (while (progn (forward-line) (not (looking-at "^$")))
            (beginning-of-line)
            (insert prefix)))
        (buffer-substring-no-properties (point-min) (point-max))))))

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
        (substitutions-list (boa-sc-snippets-query boa-ide-project-dir query))
        (root-dir boa-ide-project-dir))
    (with-current-buffer buffer
      (insert-buffer-substring-no-properties orig-buffer)
      (goto-char (point-min))
      (while changedp
        (setf changedp nil)
        (dolist (substitution substitutions-list)
          (save-match-data
            (goto-char (point-min))
            (when (re-search-forward (rx (seq (group-n 1 (* any))
                                              (group-n 2 (literal (boa-sc-substitution-target substitution)))))
                                     nil t)
              (setf changedp t)
              (message "Replacing target \"%s\"." (boa-sc-substitution-target substitution))
              (let ((replacement-string(boa-ide-prepare-replacement (match-string 1) root-dir substitution)))
                (replace-match replacement-string nil t nil 2)
                (message "Replacement made."))))))
      (message "Substitutions complete.  Enabling `boa-mode'.")
      (boa-mode)
      (read-only-mode))
    (pop-to-buffer buffer)))


;;; Completion

(defun boa-ide-complete-snippets ()
  "Offer snippet completions."
  (let* ((line-bounds (bounds-of-thing-at-point 'line))
         (line-start (car line-bounds))
         (line-end (cdr line-bounds))
         (snippets (cl-remove-duplicates (mapcar (lambda (str)
                                                   (substring str 2 (- (length str) 2)))
                                                 (boa-sc-snippets boa-ide-project-dir))
                                         :test #'string=)))
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
    (mapc (lambda (binding)
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

(declare-function c-update-modeline "cc-cmds")

(define-minor-mode boa-ide-mode
  "Provides support for Boa study template projects."
  :lighter nil
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
