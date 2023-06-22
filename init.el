;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
	  '(("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")
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
  (setq use-package-always-ensure t)
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

(use-package auto-package-update)

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
    (require 'init-ui)                    ; UI交互
    (require 'init-edit)                  ; 编辑行为
    (require 'init-org)                   ; org相关设置
    (require 'init-completion)            ; 补全系统
    (require 'init-tools)                 ; 相关工具
    (require 'init-base)                  ; 一些基本配置
    (require 'init-shell)                 ;shell设置
    (require 'init-note)                  ; 写作相关配置
    (require 'init-language)              ; 编程语言设置
    )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
