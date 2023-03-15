;;; org-shell-cat.el --- Copy block to cat command -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Hirozy
;;
;; Author: Hirozy
;; Maintainer: Hirozy
;; Created: Mar 10, 2023
;; Modified: Mar 15, 2023
;; Version: 1.0
;; Keywords: shell-cat org-mode
;; Homepage: https://github.com/Hirozy/shell-cat
;; Package-Requires: ((emacs "28.1") (org-mode "9.6"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Copy org block with cat command, this helps simplify the process from
;; the coe block to file.
;;
;;; Code:

(require 'ob-core)

(defvar org-babel-header-shell-cat-list
  '((tee . :any)
    (sudo)
    (append)
    (backslash))
  "The header args for the shell cat command.
`:tee /filepath'  -> The file path where the block will be written.
`:sudo'           -> Write with sudo permissions.
`:append'         -> Append to exise file.
`:backslash'      -> cat << EOF to cat <<\EOF.")

(defun org-babel-header-expand-shell-cat ()
  "Expand header args for shell cat."
  (dolist (pair org-babel-header-shell-cat-list)
    (add-to-list 'org-babel-common-header-args-w-values pair)))

(advice-add 'org-mode :after 'org-babel-header-expand-shell-cat)

;;;###autoload
(defun org-copy-to-shell-cat ()
  "Copy block to shell cat."
  (interactive)
  (let* ((info (org-babel-get-src-block-info 'no-eval))
         (body (nth 1 info))
         (params (nth 2 info))
         tee
         (sudo "")
         (append "")
         (backslash ""))
    (when body
      (dolist (pair params)
        (when (equal (car pair) ':tee)
          (setq tee (cdr pair)))
        (when (equal (car pair) ':sudo)
          (setq sudo "sudo "))
        (when (equal (car pair) ':append)
          (setq append "-a "))
        (when (equal (car pair) ':backslash)
          (setq backslash "\\")))
      (org-babel-mark-block)
      (if tee
          (let ((first-line (format "bash -c 'cat <<%sEOF | %stee %s\"%s\" >> /dev/null"
                                    backslash
                                    sudo
                                    append
                                    tee))
                (end-line "EOF'")
                (body (string-replace "'" "\\'" body)))
            (kill-new (format "%s\n%s\n%s" first-line body end-line))
            (message "Push block body with `shell-cat' onto the kill ring"))
        (progn
          (kill-new body)
          (message "Push block body onto the kill ring"))))))

(provide 'org-shell-cat)
