;;; package --- summary
;;; Commentary:
;;; init-lsp.el lsp-mode settings
;;; Code:

(require 'lsp-mode)
;; (add-hook 'c-mode-hook #'lsp)
;; (add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-common-hook #'lsp)

(require 'lsp-ui)


(setq lsp-ui-doc-enable nil
	  lsp-ui-sideline-enable t
	  lsp-ui-flycheck-enable t
	  lsp-ui-flycheck-list-position 'bottom
	  lsp-ui-flycheck-live-reporting t
	  lsp-ui-peek-enable nil
	  lsp-ui-peek-list-width 60
	  lsp-ui-peek-peek-height 25)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
; (add-hook 'c-mode-common-hook 'flycheck-mode)
; (add-hook 'c++-mode-hook 'flycheck-mode)


(require 'flycheck)
; (require 'lsp-ui-flycheck)
; (lsp-ui-flycheck-enable 1)

(setq lsp-prefer-flymake nil)
(add-hook 'c-mode-common-hook 'flycheck-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)
(setq company-lsp-cache-candidates nil)
(setq company-lsp-async t)
(setq company-lsp-enable-snippet t)
(setq company-lsp-enable-recompletion nil)
(push 'company-lsp company-backends)

;; show all symbols in current buffer
(global-set-key (kbd "s-2") 'rtags-imenu)

;; Set clangd arguments
(setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

(provide 'init-lsp)

;;; init-lsp.el ends here
