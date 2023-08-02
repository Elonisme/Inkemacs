;;; init-base.el -- hugo settings -*-  lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; 禁用一些GUI特性
(setq use-dialog-box nil)               ; 鼠标操作不使用对话框
(setq inhibit-default-init t)           ; 不加载 `default' 库
(setq inhibit-startup-screen t)         ; 不加载启动画面
(setq inhibit-startup-message t)        ; 不加载启动消息
(setq inhibit-startup-buffer-menu t)    ; 不显示缓冲区列表

;; 草稿缓冲区默认文字设置
(setq initial-scratch-message (concat ";; Happy hacking, "
                                      (capitalize user-login-name) " - Emacs ♥ you!\n\n"))

;; 设置缓冲区的文字方向为从左到右
(setq bidi-paragraph-direction 'left-to-right)
;; 禁止使用双向括号算法
;; (setq bidi-inhibit-bpa t)

;; 设置自动折行宽度为80个字符，默认值为70
(setq-default fill-column 80)

;; 设置大文件阈值为100MB，默认10MB
(setq large-file-warning-threshold 100000000)

;; 以16进制显示字节数
(setq display-raw-bytes-as-hex t)
;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
(setq redisplay-skip-fontification-on-input t)

;; 禁止响铃
(setq ring-bell-function 'ignore)

;; 禁止闪烁光标
(blink-cursor-mode -1)

;; 在光标处而非鼠标所在位置粘贴
(setq mouse-yank-at-point t)

;; 拷贝粘贴设置
(setq select-enable-primary nil)        ; 选择文字时不拷贝
(setq select-enable-clipboard t)        ; 拷贝时使用剪贴板

;; 鼠标滚动设置
(setq scroll-step 2)
(setq scroll-margin 2)
(setq hscroll-step 2)
(setq hscroll-margin 2)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq scroll-preserve-screen-position 'always)

;; 对于高的行禁止自动垂直滚动
(setq auto-window-vscroll nil)

;; 设置新分屏打开的位置的阈值
(setq split-width-threshold (assoc-default 'width default-frame-alist))
(setq split-height-threshold nil)

;; TAB键设置，在Emacs里不使用TAB键，所有的TAB默认为4个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; yes或no提示设置，通过下面这个函数设置当缓冲区名字匹配到预设的字符串时自动回答yes
(setq original-y-or-n-p 'y-or-n-p)
(defalias 'original-y-or-n-p (symbol-function 'y-or-n-p))
(defun default-yes-sometimes (prompt)
  "automatically say y when buffer name match following string"
  (if (or
	   (string-match "has a running process" prompt)
	   (string-match "does not exist; create" prompt)
	   (string-match "modified; kill anyway" prompt)
	   (string-match "Delete buffer using" prompt)
	   (string-match "Kill buffer of" prompt)
	   (string-match "still connected.  Kill it?" prompt)
	   (string-match "Shutdown the client's kernel" prompt)
	   (string-match "kill them and exit anyway" prompt)
	   (string-match "Revert buffer from file" prompt)
	   (string-match "Kill Dired buffer of" prompt)
	   (string-match "delete buffer using" prompt)
       (string-match "Kill all pass entry" prompt)
       (string-match "for all cursors" prompt)
	   (string-match "Do you want edit the entry" prompt))
	  t
    (original-y-or-n-p prompt)))
(defalias 'yes-or-no-p 'default-yes-sometimes)
(defalias 'y-or-n-p 'default-yes-sometimes)

;; 设置剪贴板历史长度300，默认为60
(setq kill-ring-max 200)

;; 在剪贴板里不存储重复内容
(setq kill-do-not-save-duplicates t)

;; 设置位置记录长度为6，默认为16
;; 可以使用 `counsel-mark-ring' or `consult-mark' (C-x j) 来访问光标位置记录
;; 使用 C-x C-SPC 执行 `pop-global-mark' 直接跳转到上一个全局位置处
;; 使用 C-u C-SPC 跳转到本地位置处
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)

;; 设置 emacs-lisp 的限制
(setq max-lisp-eval-depth 10000)        ; 默认值为 800
(setq max-specpdl-size 10000)           ; 默认值为 1600

;; 启用 `list-timers', `list-threads' 这两个命令
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
 ))


;; 启用xterm-mouse-mode以支持鼠标选择文本
(xterm-mouse-mode 1)

;; 启用x-clipboard集成
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; 退出Emacs时进行确认
(setq confirm-kill-emacs 'y-or-n-p)

;; 在模式栏上显示当前光标的列号
(column-number-mode t)

  ;; 配置所有的编码为UTF-8，参考：
  ;; https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (modify-coding-system-alist 'process "*" 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

  ;; 在编程模式下显示行号
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ;; 在其他模式下禁用行号显示
  (global-display-line-numbers-mode -1)

  (use-package no-littering
    :ensure t)

  (use-package saveplace
    :ensure nil
    :hook (after-init . save-place-mode))

  (use-package recentf
  :ensure nil
  :defines no-littering-etc-directory no-littering-var-directory
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  ;; `recentf-add-file' will apply handlers first, then call `string-prefix-p'
  ;; to check if it can be pushed to recentf list.
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude `(,@(cl-loop for f in `(,package-user-dir
                                           ,no-littering-var-directory
                                           ,no-littering-etc-directory)
                                collect (abbreviate-file-name f))
                     ;; Folders on MacOS start
                     "^/private/tmp/"
                     "^/var/folders/"
                     ;; Folders on MacOS end
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))

  (use-package savehist
    :ensure nil
    :hook (after-init . savehist-mode)
    :config
    ;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
    (setq enable-recursive-minibuffers t)
    (setq history-length 1000)
    (setq savehist-additional-variables '(mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history))
    (setq savehist-autosave-interval 300))

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-x K" . crux-kill-other-buffers)
         ("C-k" . crux-smart-kill-line)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-x DEL" . crux-kill-line-backwards))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

  (use-package restart-emacs
  :ensure t
  :bind
  (("C-c C-x r" . restart-emacs))
  )

(setq make-backup-files nil)                                  ; 不自动备份
(setq auto-save-default nil)                                  ; 不使用Emacs自带的自动保存

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :bind ("s-u" . revert-buffer)
  :custom
  (auto-revert-interval 10)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         )
  )

(provide 'init-base)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
