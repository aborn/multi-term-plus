;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term emacs terminal config
;; emacs terminal 终端配置，在multi-term.el的基础上进行了优化
;;   注：multi-term 采用的是 term-mode 这种模式有两种子模式
;;     一种是 (term-char-mode) 像普通的shell
;;     另一种是 (term-line-mode) 像普通的buffer
;; 见： http://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term-plus)

;; some basic configuration
(setq multi-term-program "/bin/zsh")   ;; 设置shell
(setq multi-term-buffer-name "mterm")  ;; 设置buffer名字ls

;; Use Emacs terminfo, not system terminfo, for macOS 4m
(setq system-uses-terminfo nil)

(defun multi-term-debug ()
  "Only for debug."
  (interactive)
  (if (equal (display-pixel-width) 1920)
      (message "screen width is %s" (display-pixel-width))
    (message "current os is %s. %d" system-type (display-pixel-width))))

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (select-window (multi-term-get-window-at-right-botton))   ;; 先切换到右边的窗口
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (progn (multi-term)
               (message "create a new multi-term!"))
      (progn (switch-to-buffer b)
             (message "switch a exist multi-term!"))))
  (define-key term-raw-map (kbd "M-n") 'ace-jump-mode))

(defun multi-term-get-window-at-right-botton ()
  "get the right botton window"
  (window-at (- (frame-width) 2) (- (frame-height) 6)))

(add-to-list 'term-bind-key-alist '("C-j"))
(add-to-list 'term-bind-key-alist '("C-o"))
(add-to-list 'term-bind-key-alist '("C-e"))
;;(add-to-list 'term-bind-key-alist '("M-f"))
;;(add-to-list 'term-bind-key-alist '("M-b"))
(add-to-list 'term-bind-key-alist '("C-k"))
(add-to-list 'term-bind-key-alist '("M-n"))  ;; 这句不起作用

;; for fast switch to multi-term sessions.
(global-set-key (kbd "C-{") 'multi-term-find)
(global-set-key (kbd "C-k") 'multi-term-kill-line)

;; Some hot-key im term-mode
(add-hook 'term-mode-hook
          (lambda ()
            ;; 下面设置multi-term buffer的长度无限
            (setq term-buffer-maximum-size 0)
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
            (add-to-list 'term-bind-key-alist '("C-a" . multi-term-move-beginning-of-line))
            (add-to-list 'term-bind-key-alist '("M-k" . multi-term-kill-line))
            (add-to-list 'term-bind-key-alist '("C-k" . multi-term-kill-line))
            (add-to-list 'term-bind-key-alist '("C-j" . multi-term-find))
            (add-to-list 'term-bind-key-alist '("C-d" . multi-term-delete-char))
            (add-to-list 'term-bind-key-alist '("C-b" . multi-term-backward-char))
            (add-to-list 'term-bind-key-alist '("C-f" . multi-term-forward-char))
            (add-to-list 'term-bind-key-alist '("M-l" . multi-term-expand-region))
            (setq show-trailing-whitespace nil)))

(multi-term-plus-init)
(provide 'multi-term-config)
