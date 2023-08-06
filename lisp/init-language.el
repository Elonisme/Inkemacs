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
  (setenv "WORKON_HOME" "/home/elon/.conda/envs/")
  (add-hook 'python-mode-hook 'pyvenv-mode))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

(defun my/latex-hook ()
  (interactive)
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
  (setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
  (setq TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
  (setq TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索
  ;;;LaTeX config
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))
  (add-hook 'LaTeX-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'my/latex-hook)
  ) ; 加载LaTeX模式钩子

(use-package cdlatex
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook 'org-cdlatex-mode)
  ) ;; 在 LaTeX 模式下自动开启 cdlatex

(use-package texfrag
  :ensure t
  :hook (org-mode . texfrag-mode)
  :config
  (setq texfrag-extensions '("pdf"))
  (setq texfrag-dpi 900))

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

;; Rust 开发配置
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  ;; 设置 Rust 语言的执行命令
  (setq org-babel-rust-command "rustc")
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((rust . t))))
  (add-hook 'rust-mode-hook
           (lambda () (prettify-symbols-mode))))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package ob-rust
  :ensure t)

(use-package lua-mode
  :ensure t)

(provide 'init-language)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-language.el ends here
