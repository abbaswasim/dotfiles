(require 'package)

;;; Standard package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(setq install-list '( color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow  helm-core  company  cc-mode  powerline  helm  evil zenburn-theme  powerline-evil  helm-projectile helm-helm-commands  helm-ag  evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-leader evil-indent-textobject evil-easymotion diminish company-cmake company-c-headers ag ace-jump-mode))

(package-refresh-contents)

(mapc 'package-install install-list)
