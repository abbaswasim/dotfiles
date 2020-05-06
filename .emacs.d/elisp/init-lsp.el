;;; package --- summary
;;; Commentary:
;;; init-lsp.el lsp-mode settings
;;; Code:

(require 'lsp-mode)
(add-hook 'c-mode-common-hook #'lsp)

(require 'lsp-ui)
(require 'flycheck)

(setq lsp-ui-doc-enable nil
	  lsp-ui-sideline-enable t
	  lsp-ui-flycheck-enable t
	  lsp-ui-flycheck-list-position 'bottom
	  lsp-flycheck-live-reporting t
	  lsp-enable-snippet t
	  lsp-prefer-capf t
	  lsp-diagnostic-package :flycheck
	  lsp-ui-peek-enable nil
	  lsp-ui-peek-list-width 60
	  lsp-ui-peek-peek-height 25)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'c-mode-common-hook 'flycheck-mode)

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

;; After creating the two bugs in lsp-mode I have got this advice
;; (setq lsp-completion-styles '(helm-flex)) ;; Shouldn't be required in emacs 27
;; And  "-header-insertion-decorators=0" as workarounds for now to clangd for common-complete

;; Set clangd arguments
(setq lsp-clients-clangd-args '("-j=8" "-background-index" "-header-insertion-decorators=0" "-log=error"))

;; Some LSP performance tuneing
;; Bigger limint for GC for lsp mode (100mb)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(provide 'init-lsp)

;;; init-lsp.el ends here
