;;; init-flycheck --- initialize flycheck
;;; Commentary:

;;; Code:

(require 'flycheck)
;; (require 'flycheck-popup-tip) ;; Shows very nice popup which one can control with faces but its very slow
(require 'flycheck-pos-tip) ;; The default popup library but no control over faces and now abandoned
;; (require 'flycheck-posframe) ;; New man in town, looks great, but has this annoying flicker when mode is enabled

;; flycheck enable for everything
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-delay 0.1)

;; used to show errors/warnings in tooltips instead of echo area

;; Popup tip config
;; (with-eval-after-load 'flycheck (flycheck-popup-tip-mode))

;; Postip config
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

;; Posframe config
;; (with-eval-after-load 'flycheck
  ;; (require 'flycheck-posframe)
  ;; (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; (flycheck-posframe-configure-pretty-defaults)

;; (setq flycheck-posframe-error-prefix "➤ ")
;; (setq flycheck-posframe-warning-prefix "! ")

(setq flycheck-standard-error-navigation nil)

;; Configure when the checks happens.
;; This needs to be carefully set otherwise emcacs gets unresponsive
;; Maybe in the future only do on save
;; (setq flycheck-check-syntax-automatically '(mode-enabled save new-line))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
