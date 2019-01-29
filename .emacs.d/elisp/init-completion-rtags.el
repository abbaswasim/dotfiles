;;; package --- summary
;;; Commentary:
;;; init-completion.el company-mode settings
; start company-mode with Emacs
(require 'cc-mode)
(require 'semantic)
(require 'rtags)
(require 'company)
(require 'company-c-headers)

;;; Code:

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

;; highlight all invocations of symbol under cursor
(global-semantic-idle-local-symbol-highlight-mode)

(semantic-mode 1)

;; company completion for c-headers
(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/ext")
(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/experimental")
(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/8.1.0/include")

;; rtags setup
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)

;; (setq company-backends '(company-rtags)) //Use this if you want to only use rtags for completion

(setq rtags-display-result-backend 'helm)

(global-set-key (kbd "<C-tab>") (function company-other-backend))

;; Tried and trusted company backends list for c++ code
(defun my-company-rtags-backends ()
  "Set up rtags based backends for comapany mode!"
  (interactive)
  (setq-local company-backends '((company-rtags company-semantic company-dabbrev-code company-keywords) company-c-headers company-capf)))

(add-hook 'c++-mode-hook #'my-company-rtags-backends)
(add-hook 'c-mode-hook #'my-company-rtags-backends)

;; Tried and tested company backends list for orgmode
(defun my-company-orgmode-backends ()
  "Set up orgmode backends for comapany mode!"
  (interactive)
  (setq-local company-backends '((company-ispell company-dabbrev) company-files company-capf)))

(add-hook 'org-mode-hook #'my-company-orgmode-backends)

;; (setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-tooltip-limit 20)

(setq company-idle-delay 0.3)

(global-company-mode)

;; TODO Remove this some day
;; Hack to workaround bug
;; https://github.com/syl20bnr/spacemacs/issues/11058
(defun et/semantic-remove-hooks ()
  "Utility function to remove some semantic functions!"
  (remove-hook 'completion-at-point-functions
			   'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
			   'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
			   'semantic-analyze-nolongprefix-completion-at-point-function))

(add-hook 'semantic-mode-hook #'et/semantic-remove-hooks)

;;; srefactor setup
(require 'srefactor)
(require 'srefactor-lisp)

(define-key c-mode-map (kbd "s-i") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "s-i") 'srefactor-refactor-at-point)

(provide 'init-completion-rtags)
;;; init-completion-rtags.el ends here
