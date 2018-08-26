;; init-packages.el -- elpa configuration
;; Initialize the package manager and add archives
(require 'package)

;;; Standard package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(defvar package-menu-exclude-packages '("color-theme-sanityinc-tomorrow"))

(defun package-menu--remove-excluded-packages (orig)
  "This function remove packages in ORIG from upgrading!"
  (let ((included (-filter
				   (lambda (entry)
					 (let ((name (symbol-name (package-desc-name (car entry)))))
					   (not (member name package-menu-exclude-packages))))
				   tabulated-list-entries)))
	(setq-local tabulated-list-entries included)
	(funcall orig)))

(advice-add 'package-menu--find-upgrades :around #'package-menu--remove-excluded-packages)

(provide 'init-packages)
