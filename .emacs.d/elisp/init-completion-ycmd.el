;;; package --- summary
;;; Commentary:
;;; init-completion-ycmd.el company-mode settings
; start company-mode with Emacs
;;; Code:

(require 'company)

;; Zero delay when pressing tab
(setq company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)

;; remove unused backends
(setq company-backends (delete 'company-semantic company-backends))
(setq company-backends (delete 'company-eclim company-backends))
(setq company-backends (delete 'company-xcode company-backends))
(setq company-backends (delete 'company-clang company-backends))
(setq company-backends (delete 'company-bbdb company-backends))
(setq company-backends (delete 'company-oddmuse company-backends))
(setq company-backends (delete 'company-gtags company-backends))
(setq company-backends (delete 'company-etags company-backends))
(setq company-backends (delete 'company-nxml company-backends))

;; TODO Fix this by adding the appropriate backends depending on the mode
(setq company-backends '());

(require 'rtags)

;; rtags setup
(setq rtags-display-result-backend 'helm)

(require 'ycmd)
(add-hook 'c++-mode-hook 'ycmd-mode)

;; Point ycmd to the server build using 'python2 build.py --clang-completer --ninja'
(set-variable 'ycmd-server-command '("python" "/development/ycmd/ycmd"))

(setq ycmd-force-semantic-completion t)

(require 'company-ycmd)
(company-ycmd-setup)

(require 'flycheck)

;; flycheck enable for everything
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-delay 0.1)

;; Use flycheck-popup-tip to display errors
(require 'flycheck-popup-tip)

;;; used to show errors/warnings in tooltips instead of echo area
(with-eval-after-load 'flycheck (flycheck-popup-tip-mode))
(setq flycheck-standard-error-navigation nil)

(require 'flycheck-ycmd)

(add-hook 'c-mode-common-hook 'flycheck-ycmd-setup) ;; TODO Make this work for all major modes

(provide 'init-completion-ycmd)

;;; init-completion-ycmd.el ends here
