;;; package --- summary
;;; Commentary:
;;; init-ycmd.el company-mode settings
;;; Code:

(require 'ycmd)
(add-hook 'c-mode-common-hook 'ycmd-mode)

;; Point ycmd to the server built using 'python2 build.py --clang-completer --ninja'
(set-variable 'ycmd-server-command '("python" "/development/ycmd/ycmd"))

;; Someday create a generic enough ~/ycm_extra_conf.py for ycmd
;; (set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")

;; Auto load configs without asking, basically whitelist everything
(setq ycmd-extra-conf-handler 'load)

(setq ycmd-force-semantic-completion t)

(require 'company-ycmd)
(company-ycmd-setup)

(require 'flycheck-ycmd)
(flycheck-ycmd-setup)

(require 'ycmd-eldoc)
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

(provide 'init-ycmd)

;;; init-ycmd.el ends here
