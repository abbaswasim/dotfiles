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
(require 'init-packages)
(require 'init-evil)
(require 'init-powerline)
(require 'init-helm)
(require 'init-common)
(require 'init-mappings)
(require 'init-c-cpp)

;; Remove all the distractions
(setq inhibit-splash-screen t
	  inhibit-startup-message t
	  inhibit-startup-echo-area-message t)

(add-hook 'server-visit-hook
	 (lambda ()
	   (menu-bar-mode -99)
	   (set-frame-parameter nil 'fullscreen 'fullboth)))

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(tool-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(setq undo-limit 100000)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)
;; tabs are always spaces
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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

;; Remember place where the file was last edited
(setq save-place-file "~/.emacs.d/saveplace")
(save-place-mode 1)

;; leave a few lines above and blew when scrlling
(setq scroll-margin 10 scroll-conservatively 9999 scroll-step 1)

;; Enable smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

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
 '(flycheck-posframe-error-face ((t (:background "#e37d7d" :foreground "#1d1d1d"))))
 '(flycheck-posframe-warning-face ((t (:background "#ec9562" :foreground "#1d1d1d"))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "#3e999f" :foreground "#ffffff"))))
 '(linum-relative-current-face ((t (:inherit linum :weight bold :underline "#555"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(popup-tip-face ((t (:background "khaki1" :foreground "black" :underline nil)))))

;; Enable org bullets in org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; make sure ag follows symlinks, one can also provide other command line options
(setq helm-ag-command-option "-f")

;; Stop emacs splitting windows, only use the current windows available
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;;; init.el ends here
