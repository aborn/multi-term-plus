;;; multi-term-plus.el --- An extensions plug for multi-term  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (let-alist "1.0.3") (s "1.10.0") (multi-term "1.3"))
;; Keywords: term, multi-term, multi-term-plus
;; Homepage: https://github.com/aborn/multi-term-plus
;; URL: https://github.com/aborn/multi-term-plus

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; The multi-term-plus package is used as extension for multi-term mode.
;;
;; The leanote package provides follwoing features:
;; * multi-term-find               Fast switch when you open multi-terms.
;; * multi-term-kill-line          Smart kill-line in multi-term mode.
;; * Auto recover previous term buffers
;;
;; Usage:
;; see multi-term-config.el
;;
;; Optional
;; 

;;; Code:
(require 'multi-term)
(require 'cl-lib)

(defvar multi-term-recover-alist '())

(defcustom multi-term-recovery-p t
  "Is need to recover previous term buffers when emacs bootup."
  :type 'boolean
  :group 'multi-term)

(defcustom multi-term-recover-alist-file "~/.multi-term-recover-alist"
  "Multi term alist save file."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-recover-hook '()
  "Called when each term recover."
  :type 'hook
  :group 'multi-term)

(defun multi-term-is-at-end-line ()
  "Cursor is in last line."
  (equal (line-number-at-pos) (count-lines (point-min) (point-max))))

(defun multi-term-is-term-mode ()
  "Current is term-mode"
  (eq major-mode 'term-mode))

(defun multi-term-backward-char ()
  "Backward-char in term-mode. "
  (interactive)
  (if (not (multi-term-is-term-mode))
      (backward-char)
    (progn (if (not (multi-term-is-at-end-line))
               (backward-char)
             (progn (term-send-left)
                    ;; (message "term-send-left")
                    )))))

(defun multi-term-forward-char ()
  "Forward-char in term-mode."
  (interactive)
  (if (not (multi-term-is-term-mode))
      (forward-char)
    (progn (if (not (multi-term-is-at-end-line))
               (forward-char)
             (progn (term-send-right)
                    ;; (message "term-send-right")
                    )))))

(defun multi-term-move-beginning-of-line ()
  "Smart version of move-beginning-of-line in term-mode."
  (interactive)
  (if (not (multi-term-is-term-mode))
      (beginning-of-line)
    (if (not (multi-term-is-at-end-line))
        (beginning-of-line)
      (term-send-raw))))

(defun multi-term-move-end-of-line ()
  "Smart version of move-end-of-line in term-mode."
  (interactive)
  (if (not (multi-term-is-term-mode))
      (move-end-of-line nil)
    (if (not (multi-term-is-at-end-line))
        (progn
          (move-end-of-line nil))
      (term-send-raw-string "\C-e"))))

(defun multi-term-kill-line ()
  "Smart kill-line in multi-term mode."
  (interactive)
  (if (and (eq 'term-mode major-mode)
           (multi-term-is-at-end-line))
      (term-send-raw-string "\C-k")
    (kill-line)))

(defun multi-term-delete-char ()
  "Delete char in term-mode"
  (interactive)
  (if (multi-term-is-at-end-line)
      (term-send-raw)
    (delete-char 1)))

(defun multi-term-expand-region ()
  "Wrap er/expand-region function in term-mode."
  (interactive)
  (er/expand-region 1))

(defun multi-term--buffer-name-list ()
  "Multi-term session list."
  (let* ((multi-term-filter-buffer-list
          (cl-remove-if-not
           #'(lambda (x)
               (and (member x multi-term-buffer-list)
                    (not (eq (current-buffer) x))))
           (buffer-list))))
    (mapcar (lambda (elt)
              (save-current-buffer
                (set-buffer elt)
                (let (name)
                  (setq ab/debug3 default-directory)
                  (setq name (format "%s@%s"
                                     (
                                      decode-coding-string (buffer-name elt) 'utf-8)
                                     (decode-coding-string default-directory 'utf-8)))
                  (list name elt))))
            multi-term-filter-buffer-list)))

(defun multi-term-find ()
  "Find multi-term by name, and switch it!"
  (interactive)
  (let* (collection key)
    (setq collection (multi-term--buffer-name-list))
    ;; (setq ab/debug this-command)    ;; only for debug
    (setq key (completing-read "find multi-term by name: "
                               collection))
    (let* ((buf (car (assoc-default key collection)))
           (bufwind (get-buffer-window buf)))
      (if (and bufwind (window-valid-p bufwind))
          (progn
            (message "switch to window %s" (buffer-name buf))
            (select-window bufwind))
        (when (bufferp buf)
          (message "switch to buffer %s" (buffer-name buf))
          (switch-to-buffer buf))))))

(defun multi-term-create (name)
  "Create new term `NAME'"
  (let ((old default-directory))
    (unless (file-exists-p name)
      (error "path %s does't exists, failed." name))
    (setq default-directory name)
    ;; (message "old=%s dir=%s" old default-directory)
    (multi-term)))

(defun multi-term-get-recover-alist ()
  "Produce multi-term recover alist."
  (mapcar (lambda (elt)
            (save-current-buffer
              (set-buffer elt)
              (cons (buffer-name)  default-directory)))
          multi-term-buffer-list))

(defun multi-term-save-term-alist ()
  "Save multi-term-recover-alist to file."
  (setq multi-term-recover-alist (multi-term-get-recover-alist))
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; Opened multi-term alist used for recovery.\n")
    (prin1 `(setq multi-term-recover-alist ',multi-term-recover-alist)
           (current-buffer))
    (write-region (point-min) (point-max) multi-term-recover-alist-file nil
                  'quiet)))

(defun multi-term-recover-terms ()
  "Recover multi-term previous buffers."
  (when multi-term-recovery-p
    (message "recovery multi-term previous buffers.")
    (dolist (elt multi-term-recover-alist)
      (multi-term-create (cdr elt))
      (run-hooks 'multi-term-recover-hook))))

(defun multi-term-plus-init ()
  "Recover previous term sessions when emacs bootup."
  (add-hook 'kill-emacs-hook
            'multi-term-save-term-alist)
  (when (file-readable-p multi-term-recover-alist-file)
    (load-file multi-term-recover-alist-file))
  (multi-term-recover-terms)
  (message "multi-term-plus inited."))

(provide 'multi-term-plus)
;;; multi-term-plus.el ends here
