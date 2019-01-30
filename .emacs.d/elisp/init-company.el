;;; package --- summary
;;; Commentary:
;;; init-company.el company-mode settings
; start company-mode with Emacs
;;; Code:

(require 'company)

;; Zero delay when starting completion
(setq company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-tooltip-limit 20)
;; (setq company-begin-commands '(self-insert-command))
;; (define-key company-active-map (kbd "C-n") #'company-select-next)
;; (define-key company-active-map (kbd "C-p") #'company-select-previous)

;; remove unused backends
;; (setq company-backends (delete 'company-semantic company-backends))
;; (setq company-backends (delete 'company-eclim company-backends))
;; (setq company-backends (delete 'company-xcode company-backends))
;; (setq company-backends (delete 'company-clang company-backends))
;; (setq company-backends (delete 'company-bbdb company-backends))
;; (setq company-backends (delete 'company-oddmuse company-backends))
;; (setq company-backends (delete 'company-gtags company-backends))
;; (setq company-backends (delete 'company-etags company-backends))
;; (setq company-backends (delete 'company-nxml company-backends))

;; Start with a cleaner default. Each language will add the appropriate backends depending on the mode
(setq company-backends '(company-capf company-files (company-dabbrev-code company-keywords) company-dabbrev))

(provide 'init-company)

;;; init-company.el ends here
