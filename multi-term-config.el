;;; Configurations for multi-term and multi-term-plus, modified it as your need.

;; Copyright (C) 2016 Aborn Jiang

;;; Notes:
;;  First of all, add multi-term.el and multi-term-plus.el to your load-path
;;
;;  Others: 
;;  multi-term major mode is term-mode, which has two sub-mode.
;;     one (term-char-mode) it likes common shell.
;;     another (term-line-mode) it likes common buffer.
;; Refs. http://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html

;;; Code:
(require 'multi-term)
(require 'multi-term-plus)

;; Some basic configuration
(setq multi-term-program "/bin/zsh")   ;; shell-type setting, default bash.
(setq multi-term-buffer-name "mterm")  ;; term buffer name setting.

;; Use emacs terminfo, not system terminfo, for macOS 4m
(setq system-uses-terminfo nil)

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun multi-term-use-right-botton-window ()
  "Get the right botton window."
  (window-at (- (frame-width) 2) (- (frame-height) 6)))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  ;; use right-botton window, modify as you need.
  (select-window (multi-term-use-right-botton-window))
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (progn (multi-term)
               (message "create a new multi-term!"))
      (progn (switch-to-buffer b)
             (message "switch a exist multi-term!"))))
  (define-key term-raw-map (kbd "M-n") 'ace-jump-mode))

;; Reserved key-binding, not used in multi-term mode.
(add-to-list 'term-bind-key-alist '("C-j"))
(add-to-list 'term-bind-key-alist '("C-o"))

;; for fast switch to multi-term sessions.
(global-set-key (kbd "C-{") 'multi-term-find)

;; Some hot-key im term-mode
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 0)  ;; means no limitation.
            (add-to-list 'term-bind-key-alist '("M-n" . ace-jump-mode))
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-a" . multi-term-move-beginning-of-line))
            (add-to-list 'term-bind-key-alist '("C-e" . multi-term-move-end-of-line))
            (add-to-list 'term-bind-key-alist '("C-k" . multi-term-kill-line))
            (add-to-list 'term-bind-key-alist '("C-j" . multi-term-find))
            (add-to-list 'term-bind-key-alist '("C-d" . multi-term-delete-char))
            (add-to-list 'term-bind-key-alist '("C-b" . multi-term-backward-char))
            (add-to-list 'term-bind-key-alist '("C-f" . multi-term-forward-char))
            (add-to-list 'term-bind-key-alist '("M-l" . multi-term-expand-region))
            (setq show-trailing-whitespace nil)))

;; init multi-term-plus
(multi-term-plus-init)

;; if you use ivy's ivy-completing-read for completing-read-function, add following code.
;; (add-to-list 'ivy-sort-functions-alist '(t . nil))  ;; use it as you need
;;
;; or defun your owner sort function.
;; (defun aborn/multi-term-find-sort (x y)
;;   "customize multi-term-find sort function."
;;   (message "customize the sort function.")
;;   (string< x y))
;; (add-to-list 'ivy-sort-functions-alist
;;              '(multi-term-find . aborn/multi-term-find-sort))

;; company-term not finished.
;; (require 'company-term)
;; (company-term-setup)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'multi-term-recover-hook
;;          #'(lambda ()
;;              (company-mode)
;;              (add-to-list 'company-backends 'company-term)))

;; ace-jump-mode key-binding
(define-key term-raw-map (kbd "M-n") 'ace-jump-mode)
(provide 'multi-term-config)
;;; multi-term-config.el ends here
