;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term emacs terminal config
;; emacs terminal 终端配置，在multi-term.el的基础上进行了优化
;;   注：multi-term 采用的是 term-mode 这种模式有两种子模式
;;     一种是 (term-char-mode) 像普通的shell
;;     另一种是 (term-line-mode) 像普通的buffer
;; 见： http://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)

;; 一些基本配置
(setq multi-term-program "/bin/zsh")   ;; 设置shell
(setq multi-term-buffer-name "mterm")  ;; 设置buffer名字ls
(add-to-list 'term-bind-key-alist '("C-j"))
(add-to-list 'term-bind-key-alist '("C-o"))
(add-to-list 'term-bind-key-alist '("C-e"))
;;(add-to-list 'term-bind-key-alist '("M-f"))
;;(add-to-list 'term-bind-key-alist '("M-b"))
(add-to-list 'term-bind-key-alist '("C-k"))
(add-to-list 'term-bind-key-alist '("M-n"))  ;; 这句不起作用

(defvar multi-term-recover-alist '())

(defcustom multi-term-recovery-p t
  "Is need to recover previous term buffers when emacs bootup."
  :type 'boolean
  :group 'multi-term)

(defun multi-term-is-at-end-line ()
  "判断是否在最后一行"
  (equal (line-number-at-pos) (count-lines (point-min) (point-max))))

(defun multi-term-is-term-mode ()
  "判断是否为 term 模式"
  (string= major-mode "term-mode"))

(defun multi-term-debug ()
  "debug时用"
  (interactive)
  (if (equal (display-pixel-width) 1920)
      (message "屏幕宽度为%s" (display-pixel-width))
    (message "操作系统为%s. %d" system-type (display-pixel-width))))

(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

;; 获得multi-term
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

;; 只后当是term-mode并且是最后一行时才采用 (term-send-left)
(defun multi-term-backward-char ()
  "Custom "
  (interactive)
  (if (not (multi-term-is-term-mode))
      (backward-char)
    (progn (if (not (multi-term-is-at-end-line))
               (backward-char)
             (progn (term-send-left)
                    (message "term-send-left"))))))

;; 只后当是term-mode并且是最后一行时才采用 (term-send-left)
(defun multi-term-forward-char ()
  "Custom "
  (interactive)
  (if (not (multi-term-is-term-mode))
      (forward-char)
    (progn (if (not (multi-term-is-at-end-line))
               (forward-char)
             (progn (term-send-right)
                    (message "term-send-right"))))))

;; 当处于最后一行时 "C-a" 将光标移动到 terminal开始处而不是这个行的头
(defun multi-term-move-beginning-of-line ()
  "move begin"
  (interactive)
  (if (not (multi-term-is-term-mode))
      (beginning-of-line)
    (if (not (multi-term-is-at-end-line))
        (beginning-of-line)
      (term-send-raw))))

;; optimus kill-line for term-mode
(defun multi-term-kill-line ()
  "Smart kill-line in multi-term mode."
  (interactive)
  (if (and (eq 'term-mode major-mode)
           (multi-term-is-at-end-line))
      (term-send-raw-string "\C-k")
    (kill-line)))

(defun multi-term-delete-char ()
  "delete char"
  (interactive)
  (if (multi-term-is-at-end-line)
      (term-send-raw)
    (delete-char 1)))

(defun multi-term-expand-region ()
  "Wrap er/expand-region fun."
  (interactive)
  (er/expand-region 1))

;; Use Emacs terminfo, not system terminfo, for macos 4m
;; mac系统出现了4m
(setq system-uses-terminfo nil)

;; 下面设置一些快捷键
(add-hook 'term-mode-hook
          (lambda ()
            ;; 下面设置multi-term buffer的长度无限
            (global-set-key (kbd "C-k") 'multi-term-kill-line)
            (setq term-buffer-maximum-size 0)
            (add-to-list 'term-bind-key-alist '("C-{" . multi-term-find))
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

(defun multi-term--buffer-name-list ()
  (mapcar (lambda (elt)
            (save-current-buffer
              (set-buffer elt)
              (let (name)
                (setq name (format "%s@%s" (buffer-name elt) default-directory))
                (list name elt))))
          multi-term-buffer-list))

(defun multi-term-find ()
  "Find multi-term by name, and switch it!"
  (interactive)
  (let* ((collection nil)
         (key nil))
    (setq collection (multi-term--buffer-name-list))
    (setq key (completing-read "find multi-term by name: "
                               collection))
    (let ((buf (car (assoc-default key collection))))
      (when (bufferp buf)
        (message "switch to buffer %s" (buffer-name buf))
        (switch-to-buffer buf)))))

(defun multi-term-create (name)
  "Create new term NAME"
  (let ((old default-directory))
    (setq default-directory name)
    (message "old=%s dir=%s" old default-directory)
    (multi-term)))

(defun multi-term-get-recover-alist ()
  "Product multi-term recover alist."
  (mapcar (lambda (elt)
            (save-current-buffer
              (set-buffer elt)
              (cons (buffer-name)  default-directory)
              ))
          multi-term-buffer-list))

;; multi-term-recover-alist
(defun multi-term-save-term-alist ()
  "Svae term alist."
  (aborn/log "save it")
  (setq multi-term-recover-alist (multi-term-get-recover-alist))
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; Opened multi-term alist used for recovery.\n")
    (prin1 `(setq multi-term-recover-alist ',multi-term-recover-alist)
           (current-buffer))
    (write-region (point-min) (point-max) "~/.emacs.d/.multi-term-recover-alist" nil
                  (unless arg 'quiet))))

(defun multi-term-recover-terms ()
  "Recover multi-term previous buffers."
  (when multi-term-recovery-p
    (message "recovery multi-term previous buffers.")
    (dolist (elt multi-term-recover-alist)
      (multi-term-create (cdr elt)))))

;; 初始化启动的时候打开一个terminal
(defun multi-term-init ()
  (add-hook 'kill-emacs-hook
            'multi-term-save-term-alist)
  (message "multi-term-init execute.")
  (when (file-readable-p "~/.emacs.d/.multi-term-recover-alist")
    (load-file "~/.emacs.d/.multi-term-recover-alist"))
  (multi-term-recover-terms))

(multi-term-init)
(provide 'multi-term-config)
