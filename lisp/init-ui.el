;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package ef-themes
  :ensure t
  :bind (
         ( "C-c t" . ef-themes-toggle)
         ("C-c e r" . ef-themes-load-random)
         )
  :init
  ;; set two specific themes and switch between them
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  ;; set org headings and function syntax
  (setq ef-themes-headings
        '((0 . (bold 1))
          (1 . (bold 1))
          (2 . (rainbow bold 1))
          (3 . (rainbow bold 1))
          (4 . (rainbow bold 1))
          (t . (rainbow bold 1))))
  (setq ef-themes-region '(intense no-extend neutral))
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  ;; The themes we provide are recorded in the `ef-themes-dark-themes',
  ;; `ef-themes-light-themes'.
  
  ;; 如果你不喜欢随机主题，也可以直接固定选择一个主题，如下：
  (ef-themes-select 'ef-summer)

  ;; 随机挑选一款主题，如果是命令行打开Emacs，则随机挑选一款黑色主题
  ;; (if (display-graphic-p)
  ;;     (ef-themes-load-random)
  ;;   (ef-themes-load-random 'dark))
  )

(use-package fontaine
  :ensure t
  :when (display-graphic-p)
  ;; :hook (kill-emacs . fontaine-store-latest-preset)
  :config
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "etc/fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((regular
           :default-height 140
           :default-weight regular
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.0
           )
          (large
           :default-height 180
           :default-weight normal
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.05
           )
          (t
           :default-family "Source Code Pro"
           :fixed-pitch-family "Source Code Pro"
           :variable-pitch-family "Source Code Pro"
           :italic-family "Source Code Pro"
           :variable-pitch-weight normal
           :bold-weight normal
           :italic-slant italic
           :line-spacing 0.1)
          ))
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-set-preset 'regular)

  ;; set emoji font
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji)
   (cond
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ))

  ;; set Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family
                (cond
                 ((eq system-type 'darwin)
                  (cond
                   ((member "Sarasa Mono SC Nerd" (font-family-list)) "Sarasa Mono SC Nerd")
                   ((member "PingFang SC" (font-family-list)) "PingFang SC")
                   ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                   ))
                 ((eq system-type 'gnu/linux)
                  (cond
                   ((member "Sarasa Mono SC Nerd" (font-family-list)) "Sarasa Mono SC Nerd")
                   ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                   ))
                 (t
                  (cond
                   ((member "Sarasa Mono SC Nerd" (font-family-list)) "Sarasa Mono SC Nerd")
                   ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                   )))
                )))

  ;; set Chinese font scale
  (setq face-font-rescale-alist `(
                                  ("Symbola"             . 1.3)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("Sarasa Mono SC Nerd" . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Lantinghei SC"       . 1.16)
                                  ("Kaiti SC"            . 1.16)
                                  ("Yuanti SC"           . 1.16)
                                  ))
  )


(defun set-emacsclient-font ()
  "Set the font for Emacs client frames."
  (let ((chinese-font "WenQuanYi Zen Hei")   ; 设置中文字体名称
        (english-font "Source Code Pro")        ; 设置英文字体名称
        (font-size 18)                         ; 设置字体大小
        (chinese-font-scale 1.2))               ; 设置中文字体缩放比例
    (set-face-attribute 'default nil :family english-font :height (* font-size 10))
    (set-fontset-font t 'han (font-spec :family chinese-font))
    (setq face-font-rescale-alist `(
                                     ("WenQuanYi Zen Hei"   . 1.2)
                                    ))
    )
  )

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (set-emacsclient-font))))

;; 设置窗口大小，仅仅在图形界面需要设置
(when (display-graphic-p)
  (let ((top    0)                                     ; 顶不留空
        (left   (/ (x-display-pixel-width) 10))        ; 左边空10%
        (height (round (* 0.8                          ; 窗体高度为0.8倍的显示高度
                          (/ (x-display-pixel-height)
                             (frame-char-height))))))
    (let ((width  (round (* 2.5 height))))             ; 窗体宽度为2.5倍高度
      (setq default-frame-alist nil)
      (add-to-list 'default-frame-alist (cons 'top top))
      (add-to-list 'default-frame-alist (cons 'left left))
      (add-to-list 'default-frame-alist (cons 'height height))
      (add-to-list 'default-frame-alist (cons 'width width)))))

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

;; 在命令行里支持鼠标
(xterm-mouse-mode 1)

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

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root) ; : auto
  (doom-modeline-persp-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

(use-package minions
  :ensure t
  :hook (after-init . minions-mode))

(use-package keycast
  :ensure t
  :hook (after-init . keycast-mode)
  :config
  ;; set for doom-modeline support
  ;; With the latest change 72d9add, mode-line-keycast needs to be modified to keycast-mode-line.
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-mode-line)."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line "  ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (delete '("" keycast-mode-line "  ") global-mode-string))
      ))

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (setq keycast-log-format "%-20K%C\n")
  (setq keycast-log-frame-alist
        '((minibuffer . nil)))
  (setq keycast-log-newest-first t)
  )

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p)
  :commands all-the-icons-install-fonts
  )

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below)  ; default below
  (setq shackle-default-size 0.4)          ; default 0.5
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '((compilation-mode              :ignore t)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          ("\\*corfu.*\\*"       :regexp t :ignore t)
          (helpful-mode                  :select t                          :size 0.3  :align below :popup t)
          ("*Messages*"                  :select t                          :size 0.4  :align t     :popup t)
          ("*Calendar*"                  :select t                          :size 0.3  :align t     :popup t)
          ("*info*"                      :select t                                                  :same t)
          (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
          (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
          ))
  )

(use-package popper
  :ensure t
  :bind (("M-`"     . popper-toggle-latest)
         ("M-<tab>" . popper-cycle)
         ("M-\\"    . popper-toggle-type)
         )
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          occur-mode
          pass-view-mode
          ("\\*corfu\\*" . hide)
          (compilation-mode . hide)
          ;; derived from `fundamental-mode' and fewer than 10 lines will be considered a popup
          (lambda (buf) (with-current-buffer buf
                          (and (derived-mode-p 'fundamental-mode)
                               (< (count-lines (point-min) (point-max))
                                  10))))
          )
        )
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  ;; group by project.el, projectile, directory or perspective
  (setq popper-group-function nil)
  
  ;; pop in child frame or not
  (setq popper-display-function #'display-buffer-in-child-frame)

  ;; use `shackle.el' to control popup
  (setq popper-display-control nil)
  )

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :commands (winner-undo winner-redo)
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"))
  )

(use-package dashboard
  :ensure t
  :init
  (progn
    ;; Set the title
    (setq dashboard-banner-logo-title "Welcom Inkwell Emacs")
    ;; Set the banner
    (setq dashboard-startup-banner "/home/elon/.emacs.d/logo.png")
    ;; Content is not centered by default. To center, set
    (setq dashboard-center-content t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-icon-type 'all-the-icons)
    (setq dashboard-set-file-icons t)
    )
  :config
  (dashboard-setup-startup-hook)

  ;; Customize the dashboard items
  (setq dashboard-items '((recents . 6)
                          (bookmarks . 6)
                          (agenda . 6)))
  )

;; 在编程模式下显示行号
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; 在其他模式下禁用行号显示
(global-display-line-numbers-mode -1)

  (use-package ace-window
    :ensure t
    :init
    (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    :config
    ;; 增大数字跳转显示的字体大小
    (custom-set-faces
     '(aw-leading-char-face ((t (:height 2.0 :foreground "red")))))
    :bind (("C-x o" . ace-window))
  )

(defun my-kill-buffer-and-close-window ()
  "Kill the current buffer and close its window."
  (interactive)
  (let ((current-window (selected-window))
        (buffer-to-kill (current-buffer)))
    (kill-buffer buffer-to-kill)
    (delete-window current-window)))

(global-set-key (kbd "C-x k") 'my-kill-buffer-and-close-window)

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
