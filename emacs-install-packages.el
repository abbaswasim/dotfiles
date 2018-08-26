(require 'package)

;;; Standard package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(setq install-list '(ace-jump-mode ag cmake-mode color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow company-c-headers company-cmake company-flx company-rtags company-web diminish evil-easymotion avy evil-indent-textobject evil-leader evil-mc evil-nerd-commenter evil-numbers evil-org evil-search-highlight-persist evil-surround evil-tabs elscreen evil-tutor flx flycheck-clangcheck flycheck-pos-tip flycheck-rtags helm-ag helm-company company helm-flycheck flycheck helm-helm-commands helm-projectile helm-rtags helm helm-core async highlight iedit let-alist linum-relative litable dash package-utils popup pos-tip powerline-evil powerline evil goto-chg projectile pkg-info epl restart-emacs rtags s seq smart-tab srefactor stickyfunc-enhance undo-tree web-completion-data yasnippet zenburn-theme))

(package-refresh-contents)

(mapc 'package-install install-list)
