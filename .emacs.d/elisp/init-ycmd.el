;;; package --- summary
;;; Commentary:
;;; init-ycmd.el company-mode settings
;;; Code:

(require 'ycmd)
(add-hook 'c-mode-common-hook 'ycmd-mode)

;; Point ycmd to the server build using 'python2 build.py --clang-completer --ninja'
(set-variable 'ycmd-server-command '("python" "/development/ycmd/ycmd"))

(setq ycmd-force-semantic-completion t)

(require 'company-ycmd)
(company-ycmd-setup)

(require 'flycheck-ycmd)
(flycheck-ycmd-setup)

(provide 'init-ycmd)

;;; init-ycmd.el ends here
