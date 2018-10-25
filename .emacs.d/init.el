;;; init.el --- Wasim's emacs configuration
;;; Commentary:
;;; code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Wasim Abbas")
(setq user-mail-address "abbas.wasim@gmail.com")

;; load my sexp from elisp dir
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; add custom theme folder to load themes from ~/.emacs.d/themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; load binaries from /usr/local/bin dir
(add-to-list 'exec-path "/usr/local/bin")

;; load my configurations
(require 'init-mappings)
(require 'init-packages)
(require 'init-evil)
(require 'init-powerline)
(require 'init-linum)
(require 'init-helm)
(require 'init-completion)
;; (require 'init-completion-irony)
(require 'init-common)
(require 'init-flycheck)
(require 'init-magit)
;; (require 'init-auto-complete)

;; Remove all the distractions
(setq inhibit-splash-screen t
	  inhibit-startup-message t
	  inhibit-startup-echo-area-message t)

(add-hook 'server-visit-hook
	 (lambda ()
	   (menu-bar-mode -99)
	   (set-frame-parameter nil 'fullscreen 'fullboth)))

(tool-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(setq undo-limit 100000)
(defalias 'yes-or-no-p 'y-or-n-p)
;; tabs are always spaces
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-default-style "linux" c-basic-offset tab-width)

;; string manipulation package
(require 'string-inflection)

;; let TODO done time logging
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"
				 "~/org/other.org"
				 "~/org/home.org"
				 "~/org/notes.org"))

(setq org-default-notes-file (expand-file-name "~/org/notes.org"))

;; Don't make backups everything is version controled
(setq make-backup-files nil)
(setq auto-save-default nil)

;; enable c++-mode in .h instead of c-mode and other associations
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gltf\\'" . javascript-mode))

;; Autoload code folding minor mode
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ag-insert-at-point (quote symbol))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
	(clang-format string-inflection web-completion-data undo-tree seq s restart-emacs epl pkg-info projectile goto-chg pos-tip dash let-alist highlight async helm-core flx elscreen avy litable company semantic cc-mode json saveplace package linum-off powerline linum linum-relative helm-rtags flycheck-rtags company-rtags package-utils srefactor helm flycheck evil zenburn-theme yasnippet stickyfunc-enhance powerline-evil popup iedit helm-projectile helm-helm-commands helm-gtags helm-flycheck helm-company helm-ag flycheck-pos-tip flycheck-clangcheck evil-tutor evil-tabs evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-leader evil-indent-textobject evil-easymotion diminish company-web company-flx company-cmake company-c-headers color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode ag ace-jump-mode))))

;; Show helm-kill-ring with C-k
(global-set-key (kbd "C-k") 'helm-show-kill-ring)

;; Show helm-buffer-list with
(global-set-key (kbd "s-b") 'helm-buffers-list)

;; s-w kill current buffer
(global-set-key (kbd "s-w") 'kill-this-buffer)

;; Use s-x for m-x as well
(global-set-key (kbd "s-x") 'helm-M-x)

;; Use s-r for recent files opened
(global-set-key (kbd "s-r") 'helm-recentf)

;; Use s-c for locate
(global-set-key (kbd "s-c") 'helm-locate)

;; Remember place where the file was last edited
(setq save-place-file "~/.emacs.d/saveplace")
(save-place-mode 1)

;; leave a few lines above and blew when scrlling
(setq scroll-margin 10 scroll-conservatively 9999 scroll-step 1)

;; show current function at the top
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)
(require 'stickyfunc-enhance)

;; open all gdb windows by default
(setq gdb-many-windows t)
(setq gdb-show-main t)

; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; Remove all the clutter from the status bar
(require 'diminish)
(diminish 'wrap-region-mode)
;; (diminish 'yas/minor-mode)
(diminish 'visual-line-mode)
(diminish 'undo-tree-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'hs-minor-mode)
(diminish 'color-identifiers-mode)
(diminish 'evil-mc-mode)
(diminish 'helm-mode)

;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-current-screen-face ((t (:background "#789000" :foreground "White"))))
 '(elscreen-tab-other-screen-face ((t (:background "#E4B91E" :foreground "#9166AB"))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "#3e999f" :foreground "#ffffff"))))
 '(linum-relative-current-face ((t (:inherit linum :weight bold :underline "#555"))))
 '(popup-tip-face ((t (:background "khaki1" :foreground "black" :underline nil)))))

(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display-kill-screen nil)

;; make sure ag follows symlinks, one can also provide other command line options
(setq helm-ag-command-option "-f")

;; (setq elscreen-display-tab nil)
