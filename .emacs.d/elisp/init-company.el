;;; package --- summary
;;; Commentary:
;;; init-company.el company-mode settings
; start company-mode with Emacs
;;; Code:

(require 'company)

;; Zero delay when starting completion
(setq company-idle-delay 0)
(setq company-show-numbers t)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 1)

(add-hook 'after-init-hook 'global-company-mode)

;; If can't complete common, move to the next suggestion
(define-key company-active-map [tab] 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

;; (setq company-begin-commands '(self-insert-command))
;; (define-key company-active-map (kbd "C-n") #'company-select-next)
;; (define-key company-active-map (kbd "C-p") #'company-select-previous)

;; remove unused backends
;; Start with a cleaner default. Each language will add the appropriate backends depending on the mode
(setq company-backends '(company-capf company-files (company-dabbrev-code company-keywords) company-dabbrev))

;; Fancy icons for company, not worth it, loses colors in completion and helm style fuzzy colors too
; (require 'company-box)
; (add-hook 'company-mode-hook 'company-box-mode)

;; Cycle through next backends
(global-set-key [S-tab] 'company-other-backend)

(provide 'init-company)

;;; init-company.el ends here
