;;; package --- summary
;;; Commentary:
;;; init-lsp.el lsp-mode settings
;;; Code:

(require 'lsp)
(require 'lsp-mode)
(require 'glsl-mode)

(add-hook 'c-mode-common-hook
		  (lambda ()
			(unless (eq major-mode 'glsl-mode)
			  (lsp))))

;; (add-hook 'c-mode-common-hook #'lsp)

(require 'lsp-ui)
(require 'flycheck)
(require 'lsp-diagnostics)
(require 'lsp-completion)
(require 'lsp-headerline)
(require 'lsp-icons)

(setq
 lsp-ui-doc-enable t
 lsp-ui-sideline-enable t
 lsp-ui-sideline-show-symbol t ;; check this one later what does it enable disable
 lsp-ui-sideline-ignore-duplicate t
 lsp-ui-flycheck-list-position 'bottom
 lsp-ui-peek-enable nil
 lsp-ui-peek-list-width 60
 lsp-ui-peek-peek-height 25
 lsp-enable-snippet t
 lsp-enable-imenu t
 lsp-completion-provider :capf
 lsp-diagnostics-provider :flycheck
 ;; lsp-log-io t
 ;; lsp-auto-guess-root t
 lsp-lens-enable t ;; clangd doesn't support this at the moment, but might be useful in other languages
 lsp-idle-delay 0.1
 lsp-headerline-breadcrumb-enable t
 lsp-headerline-breadcrumb-icons-enable t
 ;; lsp-semantic-tokens-enable t ;; Slightly better organisation of symbols but most of them has same color means very uniform looking, also buggy, specially for enums
 ;; lsp-headerline-breadcrumb-enable-symbol-numbers t ;; don't really need this one
 )


(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'c-mode-common-hook 'flycheck-mode)

;; show all symbols in current buffer
(global-set-key (kbd "s-2") 'helm-imenu)

(require 'evil)
;; lsp mode shortcuts for evil
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

;; Lets setup C++/C completion etc
;; Set clangd arguments
(setq lsp-clients-clangd-args '("-j=8" "-background-index" "-header-insertion-decorators=0" "-cross-file-rename" "-log=error"))

;; Some LSP performance tuneing
;; Bigger limint for GC for lsp mode (100mb)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; LSP header line setup
;; (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)) ;; default it fine

;; Lets setup swift mode server using lsp-SourceKit
(require 'lsp-sourcekit)
(setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp") ;; run 'xcrun --find sourcekit-lsp' next time to find path
(add-hook 'swift-mode-hook (lambda () (lsp))) ;; Enable swift mode when visiting .swift files

(provide 'init-lsp)

;;; init-lsp.el ends here
