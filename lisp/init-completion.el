;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

  ;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
   (setq vertico-scroll-margin 0)

  ;; Show more candidates
   (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
   (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
   (setq vertico-cycle t)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; minibuffer helpful annotations
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
          (shell-mode . corfu-mode)
          (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

(use-package eglot)

;; yasnippet settings
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((after-init . yas-reload-all)
         ((prog-mode LaTeX-mode org-mode) . yas-minor-mode))
  :config
  ;; Suppress warning for yasnippet code.
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

  (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))

(use-package all-the-icons-completion
  :ensure t
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup))
  )

(use-package embark
  :ensure t
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-'" . embark-act)
         :map minibuffer-local-map
         :map minibuffer-local-completion-map
         ("TAB" . minibuffer-force-complete)
         :map embark-file-map
         ("E" . consult-file-externally)      ; Open file externally, or `we' in Ranger
         ("O" . consult-directory-externally) ; Open directory externally
         )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)

  ;; open directory
  (defun consult-directory-externally (file)
    "Open directory externally using the default application of the system."
    (interactive "fOpen externally: ")
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
                                                                                 (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
      (call-process (pcase system-type
    	              ('darwin "open")
    	              ('cygwin "cygstart")
    	              (_ "xdg-open"))
    	            nil 0 nil
    	            (file-name-directory (expand-file-name file)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-mode t))
  )

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines t) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

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

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
