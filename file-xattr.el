;;; file-xattr.el --- Read and write extended file attributes on linux

;; Copyright (C) 2015  David Maus

;; Author: David Maus <dmaus@ictsoc.de>
;; Keywords: files

;; This file is NOT part of Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'derived)

(defcustom file-xattr-getfattr-program (executable-find "getfattr")
  "Path to `getfattr' utility for reading attributes."
  :type '(file :must-match t)
  :group 'file-xattr)

(defcustom file-xattr-setfattr-program (executable-find "setfattr")
  "Path to `setfattr' utility for writing attributes."
  :type '(file :must-match t)
  :group 'file-xattr)

(defvar file-xattr-hook-into-dired t)
(defvar file-xattr-edit-dired-key (kbd "E"))

(defvar file-xattr-edit-filename)
(defvar file-xattr-edit-attributes)

(defun file-xattr--execute (program arguments &optional noerror)
  "Execute PROGRAM with ARGUMENTS.
Signal an error if PROGRAM terminates with a non-zero exit code
unless optional argument NOERROR is set."
  (let ((exitcode (apply #'call-process program nil '(t nil) nil arguments)))
    (unless (or noerror (= 0 exitcode))
      (error "Program %s terminated with a non-zero exit code: %d" program exitcode))))

(defun file-xattr--execute-to-string (program arguments)
  "Execute PROGRAM with ARGUMENTS and return output as string."
  (with-temp-buffer
    (file-xattr--execute program arguments)
    (buffer-string)))

(defun file-xattr--parse-getfattr-output (output)
  "Return alist of attributes in OUTPUT."
  (let (attributes)
    (dolist (line (split-string output "\n" t) attributes)
      (cond
       ((string-match "^# file: \\(.+\\)$" line)
        (push (list (match-string 1 line)) attributes))
       ((string-match "^\\([^=]+\\)=\\(.+\\)$" line)
        (push (cons (match-string 1 line) (match-string 2 line)) (cdar attributes)))))))

(defun file-xattr--insert-attributes (attributes)
  "Insert ATTRIBUTES into current buffer."
  (dolist (pair attributes)
    (insert (car pair) "=" (cdr pair) "\n")))

(defun file-xattr-value-type (value)
  "Return symbol indicating type of VALUE."
  (cond
   ((string-match-p "^\"") 'string)
   ((string-match-p "^0[xX]" 'hex))
   ((string-match-p "^0[sS]" 'base64))
   (t 'unknown)))

(defun file-xattr-set (filename attribute value)
  "Set ATTRIBUTE on FILENAME to VALUE."
  (file-xattr--execute file-xattr-setfattr-program (list "-n" attribute "-v" value (expand-file-name filename))))

(defun file-xattr-get (filename attribute)
  "Return value of ATTRIBUTE on FILENAME."
  (cdar
   (file-xattr--parse-getfattr-output
    (file-xattr--execute-to-string file-xattr-getfattr-program (list "-n" attribute (expand-file-name filename))))))

(defun file-xattr-list (filenames)
  "Return list of attributes on FILENAMES."
  (file-xattr--parse-getfattr-output
   (file-xattr--execute-to-string file-xattr-getfattr-program `("-d" ,@(mapcar #'expand-file-name filenames)))))

(defun file-xattr-remove (filename attribute)
  "Remove ATTRIBUTE from FILENAME."
  (file-xattr--execute file-xattr-setfattr-program (list "-x" attribute (expand-file-name filename))))

(defconst file-xattr-edit-mode-font-lock-keywords
  '(("^\\(user\.[^=]*\\)" 1 font-lock-variable-name-face)
    ("^\\([^=]+\\)" 1 font-lock-warning-face)))

(defun file-xattr-edit (filename)
  "Edit attributes of FILENAME."
  (let ((attributes (cdar (file-xattr-list (list filename))))
        (buffer (generate-new-buffer "xattr")))
    (with-current-buffer buffer
      (insert "# Edit attributes of selected file.\n"
              "# Every attribute you delete here will be removed from the file.\n"
              "# \n"
              "# Use C-c C-c to write attributes back to file.\n"
              "# Use C-c C-k to quit editing without saving.\n"
              "# \n"
              "# File: " (expand-file-name filename) "\n"
              "# \n")
      (file-xattr--insert-attributes attributes)
      (file-xattr-edit-mode)
      (set (make-local-variable 'file-xattr-edit-filename) (expand-file-name filename))
      (set (make-local-variable 'file-xattr-edit-attributes) attributes))
    (switch-to-buffer buffer)))

(defun file-xattr-edit-quit ()
  "Quit editing without saving attributes back to file."
  (interactive)
  (when (eq major-mode 'file-xattr-edit-mode)
    (kill-buffer)))

(defun file-xattr-edit-save ()
  "Save attributes back to file and quit editing."
  (interactive)
  (when (eq major-mode 'file-xattr-edit-mode)
    (let ((attributes (cdar (file-xattr--parse-getfattr-output (buffer-string))))
          (filename file-xattr-edit-filename)
          (tempfile (make-temp-file "xattr.")))
      (dolist (attribute file-xattr-edit-attributes)
        (unless (assoc-string (car attribute) attributes)
          (file-xattr-remove file-xattr-edit-filename (car attribute))))
      (with-current-buffer (find-file-noselect tempfile)
        (insert "# file: " (expand-file-name filename) "\n")
        (file-xattr--insert-attributes attributes)
        (save-buffer)
        (kill-buffer))
      (file-xattr--execute file-xattr-setfattr-program (list (format "--restore=%s" (expand-file-name tempfile))))
      (delete-file tempfile))
    (file-xattr-edit-quit)))

(define-derived-mode file-xattr-edit-mode text-mode "Edit XAttr"
  "Major mode for editing extended file attributes."
  (setq font-lock-defaults '((file-xattr-edit-mode-font-lock-keywords) nil nil (("#" . "<") ("\n" . ">") ("\"" . "\""))))
  (define-key file-xattr-edit-mode-map (kbd "C-c C-k") #'file-xattr-edit-quit)
  (define-key file-xattr-edit-mode-map (kbd "C-c C-c") #'file-xattr-edit-save)
  (define-key file-xattr-edit-mode-map (kbd "C-c C-s") #'file-xattr-edit-save)
  (define-key file-xattr-edit-mode-map (kbd "C-x C-s") #'file-xattr-edit-save))

(defun file-xattr-hook-into-dired ()
  (define-key dired-mode-map file-xattr-edit-dired-key #'file-xattr-edit-dired))

(defun file-xattr-edit-dired ()
  (interactive)
  (let ((filename (dired-file-name-at-point)))
    (when filename
      (file-xattr-edit filename))))

(when file-xattr-hook-into-dired
  (eval-after-load 'dired #'file-xattr-hook-into-dired))

(provide 'file-xattr)

;;; file-xattr.el ends here
