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

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

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
  (setq rust-format-on-save t))

(add-hook 'rust-mode-hook
           (lambda () (prettify-symbols-mode)))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((rust . t))))

(use-package ob-rust)

;; 设置 Rust 语言的执行命令
(setq org-babel-rust-command "rustc")

(use-package lua-mode)

(provide 'init-language)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-language.el ends here
