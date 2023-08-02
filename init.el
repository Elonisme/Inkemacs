;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/stable-melpa/")
	("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")
	("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
	("gnu-devel" . "https://mirrors.ustc.edu.cn/elpa/gnu-devel/")
	("nognu-devel" . "https://mirrors.ustc.edu.cn/elpa/nongnu-devel/")
	("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-initialize)

;; 安装 `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 配置 `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (if (daemonp)
	  (setq use-package-always-demand t)))

(eval-when-compile
  (require 'use-package))

;; 安装 `use-package' 的集成模块
(use-package use-package-ensure-system-package
  :ensure t)
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

;; 安装 `quelpa'
(use-package quelpa
  :ensure t
  :commands quelpa
  :config
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-update-melpa-p nil)
  (quelpa-self-upgrade-p nil)
  (quelpa-checkout-melpa-p nil))

;; `quelpa' 与 `use-package' 集成
(use-package quelpa-use-package
  :ensure t)

(defun open-custom-post-file()
  "This functions dose finde custom-post.el."
  (interactive)
  (find-file "~/.emacs.d/emacs-config.org"))
(global-set-key (kbd "<f5>") 'open-custom-post-file)

  ;; 将lisp目录放到加载路径的前面以加快启动速度
  (let ((dir (locate-user-emacs-file "lisp")))
    (add-to-list 'load-path (file-name-as-directory dir)))

  ;; 加载各模块化配置
  ;; 不要在`*message*'缓冲区显示加载模块化配置的信息
  (with-temp-message ""
    (require 'init-base)                  ; 基础设置
    (require 'init-ui)                    ; UI交互
    (require 'init-edit)                  ; 编辑设置
    (require 'init-org)                   ; org配置
    (require 'init-complete)              ; 补全设置
    (require 'init-tools)                 ; 常用工具
    (require 'init-language)              ; 编程语言
    (require 'init-hugo)                  ; 发布博客设置
    )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lua-mode yasnippet which-key vertico toc-org texfrag swiper smartparens rust-mode rime restart-emacs quelpa-use-package pyvenv plantuml-mode ox-hugo org-super-links org-roam-ui org-modern org-contrib org-auto-tangle org-appear org-ai orderless ob-rust no-littering multiple-cursors marginalia keycast helpful good-scroll fontaine doom-themes doom-modeline diminish denote dashboard crux corfu consult-notes company cdlatex cargo blacken ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
