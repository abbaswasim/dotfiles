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

;; Check if packages are avaiable for udpate and update if interval has passed
(auto-package-update-maybe)

;; load my configurations
(require 'init-packages)
(require 'init-company)
(require 'init-flycheck)
(require 'init-evil)
(require 'init-powerline)
(require 'init-linum)
(require 'init-helm)
(require 'init-common)
(require 'init-magit)
(require 'init-neotree)
(require 'init-mappings)
(require 'init-c-cpp)
(require 'init-python)
(require 'init-cmake)
(require 'init-lsp)

;; start emacsclient server
(server-start)

;; Use simple terminal because a lot of packages struggle otherwise
(setenv "TERM" "dumb")

;; If .elc is older make sure .el is loaded and recompiled
;; TODO Check how much time this adds before enabling
;; (setq load-prefer-newer t)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Remove all the distractions
(setq inhibit-splash-screen t
	  inhibit-startup-message t
	  inhibit-startup-echo-area-message t)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; (setq ns-use-native-fullscreen t)

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

;; Autoload code folding minor mode
(add-hook 'before-save-hook 'whitespace-cleanup)

;; start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ag-insert-at-point (quote symbol) t)
 '(helm-ff-lynx-style-map t)
 '(helm-occur-use-ioccur-style-keys t)
 '(package-selected-packages
   (quote
	(posframe exec-path-from-shell lsp-ui company-lsp helm-lsp lsp-clangd solarized-theme realgud flycheck-popup-tip flycheck-ycmd company-ycmd ycmd auto-package-update org-bullets elpy neotree yasnippet-snippets clang-format string-inflection web-completion-data undo-tree seq s restart-emacs epl pkg-info projectile goto-chg pos-tip dash let-alist highlight async helm-core flx avy litable company cc-mode json saveplace package linum-off powerline linum linum-relative helm-rtags package-utils srefactor helm flycheck evil zenburn-theme yasnippet stickyfunc-enhance powerline-evil popup iedit helm-projectile helm-helm-commands helm-gtags helm-flycheck helm-company helm-ag flycheck-pos-tip evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-leader evil-indent-textobject evil-easymotion diminish company-web company-flx company-cmake company-c-headers color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode ag ace-jump-mode)))
 '(safe-local-variable-values
   (quote
	((projectile-project-compilation-cmd . "cmake --build build")
	 (project-copyright-header . "
// Roar Source Code
// Wasim Abbas
// http://www.waZim.com
// Copyright (c) 2008-2019
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the 'Software'),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// Version: 1.0.0
")
	 (project-namespace-name . "ror")))))

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

;; Enable smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

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

(when (memq window-system '(mac ns))
;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
  (setq ns-use-srgb-colorspace nil)
;;; MacOs runs emacs from gui so doesn't inherit environment
;;; Bellow lines makes sure it does
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-highlight-persist-highlight-face ((t (:background "#3e999f" :foreground "#ffffff"))))
 '(flycheck-posframe-error-face ((t (:background "#e37d7d" :foreground "#1d1d1d"))))
 '(flycheck-posframe-warning-face ((t (:background "#ec9562" :foreground "#1d1d1d"))))
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

;; enable async mode very handy
;; Try to understand how this works a bit more
(require 'async)

;; Find better shortcut keys for those
(global-set-key (kbd "s-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>")  'shrink-window)
(global-set-key (kbd "s-C-<up>")    'enlarge-window)

(when (string-equal system-type "darwin")
  ;; Non-native fullscreen
  ;; (setq ns-use-native-fullscreen nil)
  ;; delete files by moving them to the trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash"))

;;; init.el ends here
