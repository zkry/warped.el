;;; warped.el --- "modern" shell extensions -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
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

(defun warped--workspace-list-tags ())

(provide 'warped)

;;; warped.el ends here
