;;; init-language.el --- language settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; 在保存时自动格式化代码
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'py-autopep8-before-save))))

(use-package pyvenv
  :ensure t
  :after python
  :config
  (add-hook 'python-mode-hook 'pyvenv-mode))

(use-package anaconda-mode
  :ensure t
  :hook (python-mode . anaconda-mode)
  :init
  (setq anaconda-mode-eldoc-as-single-line t))

(use-package company-anaconda
  :ensure t
  :after anaconda-mode
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3") ;; 设置 Python 解释器路径
  (setq python-shell-interpreter "python3")) ;; 设置 Python 解释器路径

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

;; (use-package rust-mode)
;; (add-hook 'rust-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)))
;; (setq rust-format-on-save t)

;; ;; (add-hook 'rust-mode-hook 'eglot-ensure)
;; (add-hook 'rust-mode-hook 'lsp-deferred)

;; Rust 开发配置
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t))

(add-hook 'rust-mode-hook
           (lambda () (prettify-symbols-mode)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (rust-mode . lsp-deferred)
  :init
  (setq lsp-rust-server 'rust-analyzer))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))



(org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((rust . t))))

;; 设置 Rust 语言的执行命令
(setq org-babel-rust-command "rustc")

(use-package elisp-mode
  :ensure nil
  :after org
  :bind (:map emacs-lisp-mode-map
              ("C-c C-b" . eval-buffer)
              ("C-c C-c" . eval-to-comment)
              :map lisp-interaction-mode-map
              ("C-c C-c" . eval-to-comment)
              :map org-mode-map
              ("C-c C-;" . eval-to-comment)
              )
  :init
  ;; for emacs-lisp org babel
  (add-to-list 'org-babel-default-header-args:emacs-lisp
             '(:results . "value pp"))
  :config
  (defconst eval-as-comment-prefix " ⇒ ")
  (defun eval-to-comment (&optional arg)
    (interactive "P")
    ;; (if (not (looking-back ";\\s*"))
    ;;     (call-interactively 'comment-dwim))
    (call-interactively 'comment-dwim)
    (progn
      (search-backward ";")
      (forward-char 1))
    (delete-region (point) (line-end-position))
    (save-excursion
      (let ((current-prefix-arg '(4)))
        (call-interactively 'eval-last-sexp)))
    (insert eval-as-comment-prefix)
    (end-of-line 1))
  )

(use-package lua-mode)

(defun my/latex-hook ()
  (turn-on-cdlatex)
  (turn-on-reftex))

(use-package tex
  :ensure auctex
  :custom
  (TeX-parse-self t) ; 自动解析 tex 文件
  (TeX-PDF-mode t)
  (TeX-DVI-via-PDFTeX t)
  :config
  (setq-default TeX-master t) ; 默认询问主文件
  (add-hook 'LaTeX-mode-hook 'my/latex-hook)) ; 加载LaTeX模式钩子


(use-package cdlatex
  :after tex
  )

(provide 'init-language)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
