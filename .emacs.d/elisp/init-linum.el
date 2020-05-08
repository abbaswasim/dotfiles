;;; init-linum.el --- Stuff for line numbers.
;;; Commentary:
;;; Code:

(when (require 'linum-off)
	(add-hook 'after-change-major-mode-hook 'linum-on))

;; (require 'linum-relative)
;; (setq linum-relative-current-symbol "")
;; (linum-relative-mode 1)

(global-display-line-numbers-mode)

(provide 'init-linum)
;;; init-linum.el ends here
