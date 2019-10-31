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
	  lsp-enable-snippet t
	  lsp-ui-peek-enable nil
	  lsp-ui-peek-list-width 60
	  lsp-ui-peek-peek-height 25)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'flycheck)
; (require 'lsp-ui-flycheck)
; (lsp-ui-flycheck-enable 1)

(setq lsp-prefer-flymake nil)
(add-hook 'c-mode-common-hook 'flycheck-mode)

(require 'company-lsp)
(setq company-lsp-cache-candidates nil)
(setq company-lsp-async t)
(setq company-lsp-enable-snippet t)
(setq company-lsp-enable-recompletion nil)

;; show all symbols in current buffer
(global-set-key (kbd "s-2") 'helm-imenu)

(require 'evil)
;; lsp mode shortcuts for evil
;; (define-key evil-normal-state-map "g[" 'rtags-previous-match)
;; (define-key evil-normal-state-map "g]" 'rtags-next-match)
(define-key evil-normal-state-map "g[" 'xref-prev-line)
(define-key evil-normal-state-map "g]" 'xref-next-line)
(define-key evil-normal-state-map "gx" 'lsp-find-references)
(define-key evil-normal-state-map "gs" 'lsp-find-definition)
(define-key evil-normal-state-map "gb" 'lsp-execute-code-action)
(define-key evil-normal-state-map "gp" 'xref-pop-marker-stack)
(global-set-key (kbd "s-o") 'helm-lsp-workspace-symbol)
;; (define-key evil-normal-state-map "gp" 'rtags-preprocess-file)

;; Set clangd arguments
(setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

;; (setq lsp-log-io t)
;; Still don't know how ccls performs, could never get it to work
;; (require 'ccls)
;; (setq ccls-executable "/usr/local/bin/ccls")

(provide 'init-lsp)

;;; init-lsp.el ends here