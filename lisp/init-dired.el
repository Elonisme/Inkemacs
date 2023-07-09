;;; init-dired.el --- Dired settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package dirvish
  :ensure t
  :hook (dired-mode . dirvish-mode)
  :config
  (use-package all-the-icons-dired
    :ensure t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))
;; (dirvish-override-dired-mode)

(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
