(defvar gtd-path "")

(defun orgtd-Create ()
  "Create the Org Mode files: inbox.org, todo.org, someday.org, callman.org, reference.org, project.org."
  (interactive)
  (let ((org-directory gtd-path)) ; Replace with the actual path to the org directory
    (make-directory org-directory t)
    (let ((created-files '()))
      (dolist (file '("inbox.org" "todo.org" "someday.org" "callman.org" "reference.org" "project.org" "report.org"))
        (let ((file-path (concat org-directory file)))
          (with-temp-file file-path
            (insert "#+TITLE: " (substring file 0 -4) "\n\n"))
          (push file-path created-files)))

      (message "Created the following Org files:\n%s" (mapconcat 'identity created-files "\n")))))

(defun orgtd-Done ()
  "Cut the selected text in the current buffer and paste it into target.org."
  (interactive)
  (let ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
        (target-file (concat gtd-path "trash.org"))) ; Replace with the actual path to target.org
    (with-temp-buffer
      (insert selected-text)
      (write-region (point-min) (point-max) target-file t))
    (delete-region (region-beginning) (region-end))
    (message "DONE task move to %s" target-file)))

(defun orgtd-Move (destination)
  "Move the selected text to the specified destination Org file.
   Destination options: T for todo.org, S for someday.org, C for callman.org, R for reference.org, P for project.org."
  (interactive "cSelect destination: [T]odo, [S]omeday, [C]allman, [R]eference, [P]roject ")
  (let ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
        (target-file nil))
    (setq target-file
          (cond
           ((equal (upcase destination) ?T) (concat gtd-path "todo.org"))         ; Replace with the actual path to todo.org
           ((equal (upcase destination) ?S) (concat gtd-path "someday.org"))     ; Replace with the actual path to someday.org
           ((equal (upcase destination) ?C) (concat gtd-path "callman.org"))     ; Replace with the actual path to callman.org
           ((equal (upcase destination) ?R) (concat gtd-path "reference.org"))   ; Replace with the actual path to reference.org
           ((equal (upcase destination) ?P) (concat gtd-path "project.org"))     ; Replace with the actual path to project.org
           (t (error "Invalid destination. Aborting."))))

    (with-temp-buffer
      (insert selected-text)
      (write-region (point-min) (point-max) target-file t))
    (delete-region (region-beginning) (region-end))
    (message "GTD task move to %s" target-file)))

(provide 'orgtd)
