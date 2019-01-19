;; init-helm.el my helm configuation

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; automatically resize window to required size
(helm-autoresize-mode t)

;; always use fuzzy matching everywhere
(setq helm-mode-fuzzy-match t)
(setq completion-in-region-fuzzy-match t)

;; replace emacs M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(setq helm-buffers-fuzzy-matching t)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)

(require 'helm-misc)
(require 'helm-projectile)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)
(helm-projectile-on)

;; helm-projectile-ag would use symbol at point as input with this setting
(setq helm-ag-insert-at-point 'symbol)

(add-to-list 'projectile-other-file-alist '("inl" "cpp" "hpp" "h"))
(add-to-list 'projectile-other-file-alist '("hpp" "cpp" "inl" "h"))
(add-to-list 'projectile-other-file-alist '("cpp" "hpp" "inl" "h"))

(provide 'init-helm)
