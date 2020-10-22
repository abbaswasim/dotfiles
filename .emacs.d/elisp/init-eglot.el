;;; package --- summary
;;; Commentary:
;;; init-eglot.el eglot-mode settings
;;; Code:

;;; Start of init-eglot will make it work one day

(require 'eglot)

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd" "-log=verbose"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(require 'flycheck)

(add-hook 'c-mode-common-hook 'flycheck-mode)

;; show all symbols in current buffer
(global-set-key (kbd "s-2") 'helm-imenu)

(require 'evil)
;; (define-key evil-normal-state-map "g[" 'rtags-previous-match)
;; (define-key evil-normal-state-map "g]" 'rtags-next-match)
(define-key evil-normal-state-map "g[" 'xref-prev-line)
(define-key evil-normal-state-map "g]" 'xref-next-line)
(define-key evil-normal-state-map "gx" 'xref-find-references)
(define-key evil-normal-state-map "gs" 'xref-find-definition)
(define-key evil-normal-state-map "gb" 'eglot-code-action)
;; (define-key evil-normal-state-map "gp" 'xref-pop-marker-stack)
(global-set-key (kbd "s-o") 'helm-lsp-workspace-symbol)
;; (define-key evil-normal-state-map "gp" 'rtags-preprocess-file)

;; After creating the two bugs in lsp-mode I have got this advice
;; (setq lsp-completion-styles '(helm-flex)) ;; Shouldn't be required in emacs 27
;; And  "-header-insertion-decorators=0" as workarounds for now to clangd for common-complete

(provide 'init-eglot)

;;; init-eglot.el ends here
