;; init-magit.el --- provides magit initialization
;;; Commentary:

;;; Code:

(require 'magit)

(require 'evil-collection)
; (require 'evil-collection-magit)
;; Note to myself, just tried magit but its slow as hell
(global-set-key (kbd "s-g") 'magit-status)

(setq magit-diff-refine-hunk 'all)

(with-eval-after-load 'magit (evil-collection-magit-setup))

(provide 'init-magit)
;;; init-magit.el ends here
