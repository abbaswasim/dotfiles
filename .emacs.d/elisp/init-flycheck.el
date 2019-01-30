;;; init-flycheck --- initialize flycheck
;;; Commentary:

;;; Code:

(require 'flycheck)
(require 'flycheck-popup-tip)

;; flycheck enable for everything
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-delay 0.1)

;;; used to show errors/warnings in tooltips instead of echo area
;; Use flycheck-popup-tip to display errors
(with-eval-after-load 'flycheck (flycheck-popup-tip-mode))
(setq flycheck-standard-error-navigation nil)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
