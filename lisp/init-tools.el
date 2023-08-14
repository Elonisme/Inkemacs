;;; init-tools.el -- hugo settings -*-  lexical-binding: t -*-
;;; Commentary:

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

;; 使用 use-package 来配置 which-key
(use-package which-key
  :ensure t
  :config
  ;; 启用 which-key
  (which-key-mode)

  ;; 设置 which-key 弹出的延迟时间（可选）
  (setq which-key-idle-delay 0.5)

  ;; 设置 which-key 弹出窗口的位置（可选）
  ;; 'bottom 表示在 Emacs 窗口底部显示，'right 则在右侧显示
  (setq which-key-side-window-location 'bottom)

  ;; 设置 which-key 显示的个数（可选）
  ;; 默认为 3，如果你希望显示更多键绑定，可以增加此值
  (setq which-key-show-remaining-keys 5)

  ;; 设置 which-key 显示的特殊按键（可选）
  ;; 这里将 F1-F9、C-x和M-x 的键绑定隐藏起来
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))

  ;; 自定义 which-key 的外观（可选）
  ;; 以下是设置 which-key 弹出窗口的宽度和颜色
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-background nil)

  ;; 设置 which-key 的排除列表（可选）
  ;; 指定一些模式或命令在 which-key 弹出窗口中不显示
  ;; 这里的例子将 ivy-mode、dired-mode 和 neotree-mode 排除在外
  (setq which-key-compute-remaps nil)
  (setq which-key-compute-prefixes nil)
  (setq which-key-compute-prefixes-function 'which-key-compute-prefixes-default)

  ;; 在 minibuffer 中显示 which-key 提示（可选）
  (setq which-key-show-prefix 'bottom)
  (setq which-key-min-display-lines 6)
  
  ;; 自定义 which-key 提示的外观（可选）
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix "+")
  
  ;; 如果你希望在某些模式下禁用 which-key，可以使用下面的钩子函数（可选）
  ;; (add-hook 'some-mode-hook (lambda () (which-key-mode -1)))
  )

;; 最后加载配置
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq doc-view-resolution 300) ; 设置分辨率，根据需要进行调整

;; Check if running in Emacs server mode
(defvar running-in-server (daemonp))

(unless running-in-server
  (use-package eaf
    :load-path "~/emacs-application-framework"
    :config
    (require 'eaf-browser)
    (require 'eaf-video-player)
    (require 'eaf-file-manager)
    (require 'eaf-image-viewer)
    (require 'eaf-pdf-viewer)          ; 启用PDF阅读器
    )
)

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
  (setq org-ai-default-chat-system-prompt "You are an Emacs helper, please reply me in Org-mode format")
  (org-ai-install-yasnippets)
  )

(load-file "~/.emacs.d/keys/chatai-key.el")
(require 'chatai-key)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n u" . org-roam-ui-open)
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

(use-package rime
  :ensure t
  
  :custom
  (default-input-method "rime")

  :config
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  )

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :init
  ;; enable plantuml babel support
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((plantuml . t))))
  :config
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-plantuml-executable-path "plantuml")
  (setq plantuml-executable-path "plantuml")
  (setq plantuml-default-exec-mode 'executable)
  ;; set default babel header arguments
  (setq org-babel-default-header-args:plantuml
        '((:exports . "results")
          (:results . "file")
          ))
  )

(use-package gnuplot
  :ensure t
  :mode ("\\.gp$" . gnuplot-mode)
  :init
  (add-to-list 'org-src-lang-modes '("gnuplot" . gnuplot))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((gnuplot . t))))
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
   (setq org-babel-default-header-args:gnuplot
        '((:exports . "results")
          (:results . "file")))
  )

(use-package org-capture
  :ensure nil
  :bind ("\e\e c" . (lambda () (interactive) (org-capture)))
  :hook ((org-capture-mode . (lambda ()
                               (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
         (org-capture-mode . delete-other-windows))
  :custom
  (org-capture-use-agenda-date nil)
  ;; define common template
  (org-capture-templates `(("t" "Tasks" entry (file+headline "tasks.org" "Reminders")
                            "* TODO %i%?"
                            :empty-lines-after 1
                            :prepend t)
                           ("n" "Notes" entry (file+headline "capture.org" "Notes")
                            "* %? %^g\n%i\n"
                            :empty-lines-after 1)
                           ;; For EWW
                           ("b" "Bookmarks" entry (file+headline "capture.org" "Bookmarks")
                            "* %:description\n\n%a%?"
                            :empty-lines 1
                            :immediate-finish t)
                           ("d" "Diary")
                           ("dt" "Today's TODO list" entry (file+olp+datetree "diary.org")
                            "* Today's TODO list [/]\n%T\n\n** TODO %?"
                            :empty-lines 1
                            :jump-to-captured t)
                           ("do" "Other stuff" entry (file+olp+datetree "diary.org")
                            "* %?\n%T\n\n%i"
                            :empty-lines 1
                            :jump-to-captured t)
                           ))
  )

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
