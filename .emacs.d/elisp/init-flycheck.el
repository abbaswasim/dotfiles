;;; init-flycheck --- initialize flycheck
;;; Commentary:

;;; Code:

;;; Tried flycheck-clangcheck but its very slow hence don't use it again

(require 'flycheck)

;; flycheck enable for everything
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-delay 0.1)

;;; used to show errors/warnings in tooltips instead of echo area
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

;; (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-rtags-setup))

;; (defun my-rtags-setup()
;;   (flycheck-select-checker 'rtags)
;;   (flycheck-mode))

;; (add-hook 'c++-mode-common-hook 'my-rtags-setup)
;; (add-hook 'c-mode-common-hook 'my-rtags-setup)

(setq flycheck-standard-error-navigation nil)

;; Use rtags for flycheck highlight as well
(require 'flycheck-rtags)

;; Optional explicitly select the RTags Flycheck checker for c or c++ major mode.
;; Turn off Flycheck highlighting, use the RTags one.
;; Turn off automatic Flycheck syntax checking rtags does this manually.
(defun my-flycheck-rtags-setup ()
  "Configure flycheck-rtags for better experience."
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled new-line idle-change))
  (setq-local flycheck-highlighting-mode nil))

(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
