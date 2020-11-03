(require 'package)

;;; Standard package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(setq install-list '(cmake-font-lock lsp-sourcekit lsp-mode lsp-ui lua-mode swift-mode which-key magit posframe exec-path-from-shell lsp-mode lsp-ui company-lsp helm-lsp flycheck-posframe realgud ycmd company-ycmd flycheck-ycmd flycheck-popup-tip auto-package-update color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow company-c-headers elpy neotree yasnippet-snippets clang-format string-inflection web-completion-data undo-tree seq s restart-emacs epl pkg-info projectile goto-chg pos-tip dash let-alist highlight async helm-core flx avy litable company semantic cc-mode json saveplace package linum-off powerline linum linum-relative helm-rtags flycheck-rtags company-rtags package-utils srefactor helm flycheck evil zenburn-theme yasnippet stickyfunc-enhance powerline-evil popup iedit helm-projectile helm-helm-commands helm-gtags helm-flycheck helm-company helm-ag flycheck-pos-tip evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-leader evil-indent-textobject evil-easymotion diminish company-web company-flx company-cmake company-c-headers color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode ag ace-jump-mode))

(package-refresh-contents)

(mapc 'package-install install-list)
