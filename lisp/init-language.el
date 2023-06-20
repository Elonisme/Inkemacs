;;; init-.el --- Development settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :init
  (add-list-to-list 'org-babel-default-header-args:python '((:results . "output pp")
                                                            (:noweb   . "yes")
                                                            (:session . "py")
                                                            (:async   . "yes")
                                                            (:exports . "both")
                                                            ))
  :config
  (setq python-indent-offset 4)
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "python3"
        python-shell-prompt-detect-failure-warning nil)
  ;; disable native completion
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  )

;; need to pip install autopep8 first
(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-mode)
  )

(use-package rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))
;; (add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'lsp-deferred)

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

(use-package cdlatex)

(provide 'init-language)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
