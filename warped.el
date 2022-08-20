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
(defvar-local warped--input-field-overlays nil)
(defvar-local warped--next-command-clear-p nil)

(defun warped--self-insert-command ()
  ""
  (interactive)
  (let ((field-ov (seq-find (lambda (ov)
                              (eql (overlay-get ov 'warped-type)
                                   'warped-field))
                            (overlays-at (if (eobp) (1- (point))
                                           (point))))))
    (when (and warped--next-command-clear-p
               field-ov)
      (let ((ov-start (overlay-start field-ov))
            (ov-end (overlay-end field-ov)))
        (kill-region ov-start ov-end)
        (setq warped--next-command-clear-p nil)))
    (self-insert-command 1)))

(defconst warped--input-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'warped--next-field)
    map))

(defconst warped--field-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'warped--self-insert-command)
    (define-key map (kbd "TAB") #'warped--next-field)
    (define-key map (kbd "C-c C-c") #'warped--exit)
    map))

(defun warped--current-field-idx ()
  ""
  (catch 'done
    (let ((ovs (overlays-at (point))))
      (dolist (ov ovs)
        (when (overlay-get ov 'warped-field-idx)
          (throw 'done (overlay-get ov 'warped-field-idx))))
      nil)))

(defun warped--goto-field (idx)
  ""
  (let ((ov-start (overlay-start (nth idx warped--input-field-overlays))))
    (goto-char ov-start)))

(defun warped--exit ()
  ""
  (interactive)
  (when warped--next-command-clear-p
    (setq warped--next-command-clear-p nil))
  (when warped--input-field-overlays
    (dolist (ov warped--input-field-overlays)
      (delete-overlay ov)
      (setq warped--input-field-overlays nil))
    (setq warped--input-field-overlays nil))
  (when warped--input-overlay
    (delete-overlay warped--input-overlay)
    (setq warped--input-overlay nil)))

(defun warped--next-field ()
  ""
  (interactive)
  (let ((bol-pos (line-beginning-position)))
    (while (and (not (= (point) bol-pos))
                (not (warped--current-field-idx)))
      (forward-char -1))
    (let ((field-idx (or (warped--current-field-idx) -1)))
      (if field-idx
          (let ((next-field (mod (1+ field-idx) (length warped--input-field-overlays))))
            (warped--goto-field next-field)))))
  (setq warped--next-command-clear-p t))

(defun warped--insert-command-with-overlays (command)
  ""
  (dolist (ov warped--input-field-overlays)
    (delete-overlay ov))
  (setq warped--input-field-overlays nil)
  (insert command)
  (beginning-of-line)
  (let ((field-idx 0))
    (while (re-search-forward "{{\\(.*?\\)}}" nil t)
      (let* ((match (match-string 1))
             (match-len (length match)))
        (replace-match match)
        (save-excursion
          (forward-char (- match-len))
          (let ((ov (make-overlay (point) (+ (point) match-len) nil nil t)))
            (overlay-put ov 'face 'font-lock-warning-face)
            ;; (overlay-put ov 'evaporate t)
            (overlay-put ov 'warped-type 'warped-field)
            (overlay-put ov 'warped-field-idx field-idx)
            (overlay-put ov 'keymap warped--field-keymap)
            (cl-incf field-idx)
            (push ov warped--input-field-overlays))))))
  (setq warped--input-field-overlays (nreverse warped--input-field-overlays)))

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
      (warped--insert-command-with-overlays command)
      (goto-char start)
      (warped--goto-field 0)
      (let ((ov (make-overlay start (point-max) nil nil t)))
        ;; (overlay-put ov 'face 'font-lock-warning-face)
        (overlay-put ov 'keymap warped--input-keymap)
        (setq warped--input-overlay ov))))
  (setq warped--next-command-clear-p t))

(defun warped-action ()
  ""
  (interactive)
  (let* ((selected-tag (completing-read "select tag:" (warped--workspace-list-tags) nil t))
         (selected-action (completing-read "select action:" (warped--actions-from-tag selected-tag))))
    (warped--insert-info-string selected-action)
    (warped--insert-template selected-action)))

(insert (warped--generate-info-string "Push a tag to a remote git repository" 0))

(warped--workspace-files)
; (warped--build-cache)
(warped--workspace-list-tags)
(warped--actions-from-tag "git")

(provide 'warped)

;;; warped.el ends here
