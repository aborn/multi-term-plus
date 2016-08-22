;;; company-term.el --- An company plug for multi-term  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Aborn Jiang

;;; Code:
;; 
;; TODO on development.

(require 'cl-lib)
(require 'company)

(defconst term-completions
  '("ls" "cp" "mv" "rm" "ls -a" "df -h" "df" "ls -ltrh"))

(defun company-term--prefix ()
  "Prefix-command handler for the company backend."
  (and (eq major-mode 'term-mode)
       (company-grab-symbol)))

(defun company-term--post-completion (candidate)
  "Insert function arguments after completion for CANDIDATE."
  ;;(term-send-raw-string candidate)
  ;;(term-char-mode)
  (message "%s" candidate))

(defun company-term (command &optional arg &rest ignored)
  "company-term."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-term))
    (prefix (company-term--prefix))
    ;;(prefix (and (eq major-mode 'term-mode)
    ;;(company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      term-completions))
    (meta (format "%s" arg))
    (post-completion (company-term--post-completion arg))
    ))

;;;###autoload
(defun company-term-setup ()
  "add to list"
  (add-to-list 'company-backends 'company-term))

(provide 'company-term)
;;; company-term.el ends here
