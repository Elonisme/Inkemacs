;;; init-hugo.el -- hugo settings -*-  lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(defun execute-hugo-deploy-script (args)
  "执行 deploy.sh 脚本文件并将结果输出到 *Shell Output* 缓冲区"
  (interactive "sArguments: ")
  (let ((script-file "/home/elon/hugo_blog/blogsite/deploy.sh"))
    (if (file-exists-p script-file)
        (progn
          (message "Running deploy.sh script...")
          (shell-command (concat script-file " " args) "*Shell Output*"))
      (message "deploy.sh script file not found"))))

(provide 'init-hugo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
