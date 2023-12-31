;;; init-ui.el -- hugo settings -*-  lexical-binding: t -*-
;;; Commentary:

;;; Code:

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
    (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
    (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    )
  :config
  (dashboard-setup-startup-hook)

  ;; Customize the dashboard items
  (setq dashboard-items '((recents . 6)
                          (bookmarks . 6)
                          ))
  )

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

(use-package nyan-mode
  :ensure t
  :init (nyan-mode))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (display-time-mode t)
  (setq doom-modeline-time t)
 )

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-moonlight t)
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
	     :default-height 150
	     :default-weight normal
	     :fixed-pitch-height 1.0
	     :variable-pitch-height 1.0
	     )
	    (large
	     :default-height 250
	     :default-weight normal
	     :fixed-pitch-height 1.0
	     :variable-pitch-height 1.05
	     )
	    (t
	     :default-family "IntelOne Mono"
         :fixed-pitch-family "IntelOne Mono"
         :variable-pitch-family "IntelOne Mono"
         :italic-family "IntelOne Mono"
         :default-weight normal
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
     (if (version< emacs-version "28.2")
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
		     ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
		     ((member "PingFang SC" (font-family-list)) "PingFang SC")
		     ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
		     ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
		     ))
		   ((eq system-type 'gnu/linux)
		    (cond
		     ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
		     ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")		     
		     ))
		   (t
		    (cond
		     ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
		     ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
		     )))
		  )))
    
    ;; set font scale
 (setq face-font-rescale-alist `(
                                  ("Symbola"             . 1.3)
                                  ("Microsoft YaHei"     . 1.2)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("Sarasa Term SC Nerd" . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Lantinghei SC"       . 1.16)
                                  ("Kaiti SC"            . 1.16)
                                  ("Yuanti SC"           . 1.16)
                                  ("Apple Color Emoji"   . 0.91)
                                  ))
    
 )

(use-package nerd-icons
  :ensure t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode)
  )

(use-package nerd-icons-completion
  :ensure t
  :hook
  (after-init . nerd-icons-completion-mode)
  )

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package treemacs
  :ensure t
  :init
  (setq treemacs-follow-mode t)
  :config
  (treemacs-resize-icons 18)) ; 调整图标大小（可选）

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
