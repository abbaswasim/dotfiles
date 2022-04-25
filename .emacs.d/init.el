;;; init.el --- Wasim's emacs configuration
;;; Commentary:
;;; code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq user-full-name "Wasim Abbas")
(setq user-mail-address "abbas.wasim@gmail.com")

;; load my sexp from elisp dir
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; add custom theme folder to load themes from ~/.emacs.d/themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(when (memq window-system '(mac ns))
;;; MacOs runs emacs from gui so doesn't inherit environment
;;; the following lines makes sure it does
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; load my configurations
(require 'init-packages)
(require 'init-company)
(require 'init-flycheck) ;; need to enable to python and other stuff, for C/C++ lsp-mode takes care of this
(require 'init-evil)
(require 'init-powerline)
(require 'init-linum)
(require 'init-helm)
(require 'init-common)
;; (require 'init-magit)
(require 'init-neotree)
(require 'init-mappings)
(require 'init-c-cpp)
(require 'init-python)
(require 'init-cmake)
(require 'init-lsp)
;; (require 'init-dap)
(require 'init-orgmode)
;; (require 'init-eglot)

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

;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))
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
 '(c-noise-macro-names '("FORCE_INLINE" "ROAR_ENGINE_ITEM"))
 '(evil-search-module 'evil-search)
 '(evil-undo-system 'undo-redo)
 '(evil-want-Y-yank-to-eol t)
 '(helm-ag-insert-at-point 'symbol t)
 '(helm-ff-lynx-style-map t)
 '(helm-occur-use-ioccur-style-keys t)
 '(org-agenda-window-setup 'current-window)
 '(package-selected-packages
   '(powerline powerline-evil lsp-sourcekit lsp-ui helm-lsp lsp-mode helm-rg cmake-font-lock lua-mode swift-mode which-key posframe exec-path-from-shell lsp-clangd solarized-theme realgud flycheck-popup-tip auto-package-update org-bullets elpy neotree yasnippet-snippets clang-format string-inflection web-completion-data seq s restart-emacs epl pkg-info projectile goto-chg pos-tip dash let-alist highlight async helm-core flx avy litable company cc-mode json saveplace package linum package-utils srefactor helm flycheck evil zenburn-theme yasnippet popup iedit helm-projectile helm-helm-commands helm-gtags helm-flycheck helm-company helm-ag flycheck-pos-tip evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-leader evil-indent-textobject evil-easymotion diminish company-web company-flx company-cmake company-c-headers color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode ag ace-jump-mode))
 '(projectile-globally-ignored-directories '(".idea" ".vscode" ".git" ".hg"))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "cd /System/Volumes/Data/personal/vulkaned && cmake -H. -Bbuild -DCMAKE_BUILD_TYPE=Debug && cmake --build build --config Debug -- -j$NUMCPUS")
	 (projectile-project-compilation-cmd . "cd /System/Volumes/Data/personal/roar_engine && cmake -H. -Bbuild -DCMAKE_BUILD_TYPE=Debug && cmake --build build --config Debug -- -j$NUMCPUS")
	 (project-namespace-name . "ror")
	 (project-copyright-header . "
// Roar Source Code
// Wasim Abbas
// http://www.waZim.com
// Copyright (c) 2021
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
")))
 '(undo-limit 10000000)
 '(undo-strong-limit 10000000))

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
;; (setq scroll-margin 10 scroll-conservatively 101 scroll-step 1)
;; (setq scroll-margin 10 scroll-conservatively 101 scroll-step 1)
;; (setq scroll-conservatively 101)
;;   scroll-conservatively 10000
;;   scroll-preserve-screen-position 1)

(setq
 scroll-conservatively 1000                     ;; only 'jump' when moving this far
 scroll-margin 4                                ;; scroll N lines to screen edge
 scroll-step 1                                  ;; keyboard scroll one line at a time
 mouse-wheel-scroll-amount '(6 ((shift) . 1))   ;; mouse scroll N lines
 mouse-wheel-progressive-speed nil              ;; don't accelerate scrolling

 ;; redisplay-dont-pause t                         ;; don't pause display on input

 ;; Always redraw immediately when scrolling,
 ;; more responsive and doesn't hang!
 fast-but-imprecise-scrolling nil
 jit-lock-defer-time 0
 )

;; Enable smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; open all gdb windows by default
(setq gdb-many-windows t)
(setq gdb-show-main t)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;; Which key
(require 'which-key)
(which-key-mode)

;; Remove all the clutter from the status bar
(require 'diminish)
(diminish 'wrap-region-mode)
;; (diminish 'yas/minor-mode)
(diminish 'visual-line-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'hs-minor-mode)
(diminish 'color-identifiers-mode)
(diminish 'evil-mc-mode)
(diminish 'helm-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-posframe-error-face ((t (:background "#e37d7d" :foreground "#1d1d1d"))))
 '(flycheck-posframe-warning-face ((t (:background "#ec9562" :foreground "#1d1d1d"))))
 '(linum-relative-current-face ((t (:inherit linum :weight bold :underline "#555"))))
 '(lsp-face-highlight-read ((t (:inherit highlight :background "dark olive green" :underline t))))
 '(lsp-face-highlight-textual ((t (:inherit highlight :background "black"))))
 '(lsp-face-highlight-write ((t (:inherit highlight :background "saddle brown" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(popup-tip-face ((t (:background "khaki1" :foreground "black" :underline nil)))))

;; make sure ag follows symlinks, one can also provide other command line options
(setq helm-ag-command-option "-f")

;; Stop emacs splitting windows, only use the current windows available
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; Compilation buffer scroll behaviour set to last command
(setq compilation-scroll-output t)
;; (setq compilation-scroll-output 'first-error) ;; Also possible but I prefer to end of output

;; enable async mode very handy
;; Try to understand how this works a bit more
(require 'async)

;; Find better shortcut keys for those
(global-set-key (kbd "s-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>")  'shrink-window)
(global-set-key (kbd "s-C-<up>")    'enlarge-window)

(desktop-save-mode 1)

;; (when (string-equal system-type "darwin")
;;   (setq frame-resize-pixelwise t)

;;   ;; On Big mac open a bigger window please, perhaps guess the resolution in the future
;;   (setq default-frame-alist '((left . 0) (width . 363) (fullscreen . fullheight)))
;;   (add-to-list 'default-frame-alist '(undecorated . t))

;;   ;; Non-native fullscreen
;;   ;; (setq ns-use-native-fullscreen nil)
;;   ;; delete files by moving them to the trash
;;   (setq delete-by-moving-to-trash t)
;;   (setq trash-directory "~/.Trash"))

;; (when (string-equal system-type "gnu/linux")
;;   (add-to-list 'default-frame-alist '(height . 79))
;;   (add-to-list 'default-frame-alist '(width . 280))
;;   (when (not (display-graphic-p))
;;	(progn
;;	 (cl-loop for char from ?1 to ?9
;;			  do (define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "s-%c" char))))

;;	 (cl-loop for char from ?a to ?z
;;			  do (define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "s-%c" char))))
;;	 )))

;;; init.el ends here
