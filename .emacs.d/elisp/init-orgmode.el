;;; init-orgmode.el --- Stuff for Org-Mode
;;; Commentary:
;;; Code:

;; Enable org bullets in org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key [f16]
  (lambda () (interactive) (org-capture nil "t")))

;; let TODO done time logging
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"
							 "~/org/other.org"
							 "~/org/home.org"
							 "~/org/notes.org"))

(setq org-default-notes-file (expand-file-name "~/org/notes.org"))


(provide 'init-orgmode)
;;; init-orgmode.el ends here
