;;; package --- summary
;;; Commentary:
;;; init-completion-irony.el company-mode settings
; start company-mode with Emacs

(require 'cc-mode)
(require 'semantic)
;; (require 'rtags)
(require 'company)
(require 'company-c-headers)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

;; highlight all invocations of symbol under cursor
(global-semantic-idle-local-symbol-highlight-mode)

(semantic-mode 1)

;; company completion for c-headers
(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
(add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.0.2/include")
;; (add-to-list 'flycheck-clang-include-path "Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.0.2/include")

;; TODO: add user paths
;; (add-to-list 'company-c-headers-path-user "/some/user/paths")

;; rtags setup
;; (setq rtags-autostart-diagnostics t)
;; (rtags-diagnostics)
;; (setq rtags-completion-enabled t)
;; (setq rtags-completions-enabled t)
;; (setq rtags-use-helm t)

(global-set-key (kbd "<C-tab>") (function company-other-backend))

;; (add-hook 'c++-mode-hook
            ;; (lambda ()
              ;; (set (make-local-variable 'company-backends)
                   ;; '((company-dabbrev-code company-rtags)))))

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-tooltip-limit 20)

(setq company-backends (delete 'company-semantic company-backends))
(setq company-backends (delete 'company-clang company-backends))

(push '(company-dabbrev-code company-keywords) company-backends)
(add-to-list 'company-backends 'company-c-headers)
;; (setq company-backends '(company-rtags))

(global-company-mode)

;; https://github.com/Andersbakken/rtags/issues/310
;; Using company-rtags asynchronously doesn't work, its currently invoked with tab
;; (setq company-rtags-use-async nil)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            ;; (company-begin-backend 'company-rtags)
            (company-begin-backend 'company-complete)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)

;;; Irony mode setup
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))

(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(provide 'init-completion-irony)
