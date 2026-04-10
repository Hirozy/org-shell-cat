;;; org-shell-cat.el --- Copy block to cat command -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Hirozy
;;
;; Author: Hirozy
;; Maintainer: Hirozy
;; Created: Mar 10, 2023
;; Modified: APr 10, 2026
;; Version: 1.3
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

;; Copy org-mode source blocks to kill ring, optionally wrapped in
;; shell cat/tee commands for writing to files.
;;
;; Supported header args:
;;   :tee "/path/to/file"  - Write block content to file
;;   :sudo                 - Write with sudo permissions
;;   :append               - Append instead of overwrite
;;   :nobackslash          - Enable variable expansion in heredoc
;;
;; By default, heredoc uses `<<\EOF' to prevent variable expansion,
;; which is safer for config files. Use `:nobackslash' to allow expansion.
;;
;; Commands:
;;   `org-copy-src-block'          - Copy block content only
;;   `org-copy-to-shell-cat'       - Copy with tee if :tee present
;;   `org-copy-to-shell-cat-prompt' - Copy with interactive prompts

;;; Code:

(require 'org)
(require 'ob-core)

(defgroup org-shell-cat nil
  "Copy org source blocks as shell cat commands."
  :group 'org-babel
  :prefix "org-shell-cat-")

(defcustom org-shell-cat-heredoc-marker "EOF"
  "The heredoc marker to use."
  :type 'string
  :group 'org-shell-cat)

(defvar org-babel-header-shell-cat-list
  '((tee . :any)
    (sudo)
    (append)
    (nobackslash))
  "Header arguments for shell cat commands.
- `:tee PATH'       - File path to write content.
- `:sudo'           - Write with sudo permissions.
- `:append'         - Append to existing file.
- `:nobackslash'    - Enable variable expansion in heredoc.")

;;;###autoload
(defun org-babel-header-expand-shell-cat ()
  "Register shell-cat header arguments with org-babel."
  (dolist (pair org-babel-header-shell-cat-list)
    (cl-pushnew pair org-babel-common-header-args-w-values :test #'equal)))

(defun org-shell-cat--param-present-p (params key)
  "Check if KEY is present in PARAMS."
  (assq key params))

(defun org-shell-cat--escape-single-quotes (str)
  "Escape single quotes in STR for shell single-quoted strings."
  (replace-regexp-in-string "'" "'\\\\''" str))

(defun org-shell-cat--escape-filepath (path)
  "Escape special characters in PATH for shell."
  (replace-regexp-in-string "\"" "\\\\\"" path))

(defun org-shell-cat--get-block-body ()
  "Get the body of the current source block.
Returns nil if not in a source block or block is empty."
  (unless (org-in-src-block-p)
    (user-error "Not in a source block"))
  (let* ((info (org-babel-get-src-block-info 'no-eval))
         (body (nth 1 info)))
    (unless body
      (user-error "Empty source block"))
    body))

(defun org-shell-cat--build-command (body tee &optional sudo append nobackslash)
  "Build shell cat command writing BODY to TEE.
SUDO, APPEND are optional modifiers.
NOBACKSLASH if non-nil enables variable expansion (removes backslash)."
  (let* ((escaped-body (org-shell-cat--escape-single-quotes body))
         (escaped-tee (org-shell-cat--escape-filepath tee))
         (marker org-shell-cat-heredoc-marker)
         (heredoc-marker (if nobackslash marker (concat "\\" marker))))
    (format "bash -c 'cat <<%s | %stee %s\"%s\" > /dev/null\n%s\n%s'"
            heredoc-marker
            (if sudo "sudo " "")
            (if append "-a " "")
            escaped-tee
            escaped-body
            marker)))

;;;###autoload
(defun org-copy-src-block ()
  "Copy source block content to kill ring.

Simply copies the block body without any tee command wrapping,
ignoring all header arguments."
  (interactive)
  (let ((body (org-shell-cat--get-block-body)))
    (kill-new body)
    (message "Copied block body to kill ring")))

;;;###autoload
(defun org-copy-to-shell-cat ()
  "Copy source block to kill ring, optionally as a shell cat command.

When `:tee' header is present, generates a shell command that writes
the block content to the specified file.

By default, uses `<<\\EOF' to prevent variable expansion.
Add `:nobackslash' to enable variable expansion."
  (interactive)
  (let* ((body (org-shell-cat--get-block-body))
         (info (org-babel-get-src-block-info 'no-eval))
         (params (nth 2 info))
         (tee (alist-get :tee params)))
    (if tee
        (let ((command (org-shell-cat--build-command
                        body
                        tee
                        (org-shell-cat--param-present-p params :sudo)
                        (org-shell-cat--param-present-p params :append)
                        (org-shell-cat--param-present-p params :nobackslash))))
          (kill-new command)
          (message "Copied as shell-cat command: tee → %s" tee))
      (kill-new body)
      (message "Copied block body to kill ring"))))

;;;###autoload
(defun org-copy-to-shell-cat-prompt ()
  "Copy source block with prompted file path."
  (interactive)
  (let* ((body (org-shell-cat--get-block-body))
         (tee (read-file-name "Write to file: "))
         (sudo (y-or-n-p "Use sudo? "))
         (append (y-or-n-p "Append to file? "))
         (nobackslash (y-or-n-p "Enable variable expansion? ")))
    (let ((command (org-shell-cat--build-command body tee sudo append nobackslash)))
      (kill-new command)
      (message "Copied as shell-cat command: tee → %s" tee))))

(provide 'org-shell-cat)

;;; org-shell-cat.el ends here
