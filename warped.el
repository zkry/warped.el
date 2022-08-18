;;; warped.el --- "modern" shell extensions -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/zkry/warped.el
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'yaml)

(defcustom warped-workspace-root nil
  "Root directory to search for workspaces in."
  :type 'string)

(defconst warped-error-buffer-name "*warped-errors*")

(defvar warped-spec-cache nil)

(setq warped-workspace-root "/Users/zromero/dev/emacs/Workflows/specs")

(defun warped--workspace-files (&optional directory)
  "Return a list of filepaths for all workspace files."
  (let ((file-paths (f-entries (or directory warped-workspace-root)))
        (result '()))
    (dolist (file-path file-paths)
      (when (and (f-file? file-path)
                 (or (string-suffix-p "yaml" file-path)
                     (string-suffix-p "yml" file-path)))
        (push file-path result))
      (when (and (not (equal "." file-path))
                 (not (equal ".." file-path))
                 (not (f-symlink? file-path))
                 (f-dir? file-path))
        (let ((children-ress (warped--workspace-files file-path)))
          (setq result (append result children-ress)))))
    result))

(defun warped--build-cache ()
  "Cache the parsed contents of workspace files."
  (setq warped-spec-cache (make-hash-table :test #'equal))
  (let ((files (warped--workspace-files)))
    (dolist (file files)
      (let ((file-contents))
        (with-temp-buffer
          (condition-case err
              (progn
                (insert-file-contents file)
                (setq file-contents (yaml-parse-string (buffer-string))))
              (error
               (with-current-buffer warped-error-buffer-name
                 (goto-char (point-max))
                 (insert "\nUnable to parse file: " file)))))
        (puthash (gethash 'name file-contents) file-contents warped-spec-cache)))))

(defun warped--actions-from-tag (tag)
  "Return a list of all actions for a given TAG."
  (let ((actions '()))
    (maphash (lambda (name ht)
               (let ((tags (gethash 'tags ht [])))
                 (when (seq-contains tags tag #'equal)
                   (push name actions))))
             warped-spec-cache)
    (nreverse actions)))

(defun warped--workspace-list-tags ()
  "Return a list of all defined tags."
  (let ((tags '()))
    (maphash (lambda (_ ht)
              (setq tags (append tags (seq-into (gethash 'tags ht '()) 'list))))
             warped-spec-cache)
    (delete-duplicates tags :test #'string-equal)
    tags))

(defun warped--generate-info-string (action &optional selected-arg)
  "Generate info string for ACTION.

If provided, SELECTED-ARG is the index of the argument being edited."
  (let* ((template-data (gethash action warped-spec-cache))
         (command (gethash 'command template-data))
         (description (gethash 'description template-data))
         (arguments (gethash 'arguments template-data))
         (source-url (gethash 'source_url template-data))
         (selected-arg-name))
    (when selected-arg
      (setq selected-arg-name (gethash 'name (aref arguments selected-arg))))
    (with-temp-buffer
      (insert "\n\n")
      (insert action "\n")
      (insert "\n")
      (insert "    " command "\n")
      (insert "\n")
      (insert "    " description "\n")
      (seq-do
       (lambda (arg)
         (let* ((name (gethash 'name arg))
                (is-selected (string-equal name selected-arg-name))
                (description (gethash 'description arg))
                (default-val (gethash 'default_value arg)))
           (if is-selected
               (insert "      " (propertize name 'font-lock-face 'font-lock-warning-face) ": " description)
             (insert "      " name ": " description))))
       arguments)
      (insert "\n\n")
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "{{\\(.*?\\)}}" nil t)
          (let ((match (match-string 1)))
            (if (equal match selected-arg-name)
                (replace-match (propertize match 'font-lock-face 'font-lock-warning-face))
              (replace-match match)))))
      (buffer-string))))

(defvar-local warped--info-overlay nil)

(defun warped--insert-info-string (action &optional selected-arg)
  ""
  (let ((info-str (warped--generate-info-string action selected-arg)))
    (goto-char (point-max))
    (forward-line 0)
    (let ((start (point)))
      (when warped--info-overlay
        (delete-overlay warped--info-overlay))
      (insert info-str)
      (setq warped--info-overlay (make-overlay start (point)))
      (overlay-put warped--info-overlay 'face 'highlight))))

(defvar-local warped--input-overlay nil)

(defun warped--insert-template (action)
  ""
  (when warped--input-overlay
    (delete-overlay warped--input-overlay)
    (setq warped--input-overlay nil))
  (let* ((template-data (gethash action warped-spec-cache))
         (command (gethash 'command template-data)))
    (let ((start (save-excursion (goto-char (point-max)) (beginning-of-line) (point)))
          (end (save-excursion (goto-char (point-max)) (point))))
      (kill-region start end)
      (goto-char start)
      (insert command)
      (let ((ov (make-overlay start (point-max) nil nil t)))
        (overlay-put ov 'face 'font-lock-warning-face)
        (setq warped--input-overlay ov)))))

(insert (warped--generate-info-string "Push a tag to a remote git repository" 0))

(warped--workspace-files)
(warped--build-cache)
(warped--workspace-list-tags)
(warped--actions-from-tag "git")

(provide 'warped)

;;; warped.el ends here
