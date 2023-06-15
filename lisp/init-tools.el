;;; init-tools.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-mode)
  :bind (([remap describe-command] . helpful-command)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h s" . helpful-symbol)
         ("C-h S" . describe-syntax)
         ("C-h m" . describe-mode)
         ("C-h F" . describe-face)
         ([remap describe-key] . helpful-key))
  )

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c @" "hideshow"
    "C-c i" "ispell"
    "C-c t" "hl-todo"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x t" "tab")
  :custom
  (which-key-idle-delay 0.7)
  (which-key-add-column-padding 1))

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :bind (
         ("C-c q" . org-ai-prompt)
         ("C-c x" . org-ai-on-region)
         )
  :hook (org-mode . org-ai-mode)
  :config
  (setq org-ai-default-max-tokens 480)
  (setq org-ai-default-chat-system-prompt "你是一个Emacs助手，请以Org-mode的格式来回复我")
  (org-ai-install-yasnippets)
  )

(load-file "~/.emacs.d/keys/chatai-key.el")
(require 'chatai-key)

(use-package org-noter
  :ensure t
  :custom
  (org-noter-notes-search-path '("~/Documents/Notes")) ;; 默认笔记路径
  (org-noter-auto-save-last-location t) ;; 自动保存上次阅读位置
  (org-noter-highlight-selected-text t)
  (org-noter-max-short-selected-text-length 20) ;; 默认为 80
  (org-noter-default-heading-title "第 $p$ 页的笔记") ;; 默认短标题格式
  :bind
  (("C-c n n" . org-noter) ;; 与 org-roam 配合
   :map org-noter-doc-mode-map ;; 加入左手键位
   ("e" . org-noter-insert-note)
   ("M-e" . org-noter-insert-precise-note)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map) ;; 日记菜单
  :config
  (require 'org-roam-dailies)
  ;;============= 新增内容 =================
  ;; 下面的 (setq my/ref-template ...) 可以放到 use-package 代码块之外
  (setq my/ref-template
        (concat "#+FILETAGS: reading research \n"
                "- tags :: %^{keywords} \n"
                "* %^{title}\n"
                ":PROPERTIES:\n"
                ":Custom_ID: %^{citekey}\n"
                ":URL: %^{url}\n"
                ":AUTHOR: %^{author-or-editor}\n"
                ":NOTER_DOCUMENT: ~/Nutstore Files/zotero-lib/%^{citekey}.pdf\n"
                ":NOTER_PAGE:\n"
                ":END:"))
  (add-to-list 'org-roam-capture-templates
               `("r" "Zotero 文献模板" plain ; 文献笔记模板
                 ,my/ref-template
                 :target
                 (file+head "ref/${citekey}.org" "#+title: ${title}\n")))

  (setq org-roam-capture-templates
        '(("n" "笔记" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)))


  ;;============= 新增内容结束 =================
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;;(require 'org-roam-protocol)
  )

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow-mode t)
  )

(use-package restart-emacs
  :ensure t
  :bind
  (("C-c C-a r" . restart-emacs))
  )

(use-package rime
  :custom
  (default-input-method "rime"))

(global-set-key (kbd "C-\\") 'toggle-input-method)

(use-package pyvenv-auto)
(setenv "WORKON_HOME" "/home/elon/anaconda3/envs/")

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
