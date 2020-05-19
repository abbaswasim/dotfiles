;;; init-c-cpp --- initialize cmake
;;; Commentary:

;;; Code:

;; (require 'cl)

;; enable c++-mode in .h instead of c-mode and other associations
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gltf\\'" . javascript-mode))

;; Autoload code folding minor mode
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)

(require 'company-c-headers)

;; company completion for c-headers
;; system dirs (for include <...>)
;; -> use "gcc -E -Wp,-v -" to get the complete list
;; Not required anymore, lsp takes care of this
;; (add-to-list 'company-c-headers-path-system "/usr/local/include")
;; (add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
;; (add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
;; (add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/ext")
;; (add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/experimental")
;; (add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/*/include")

;; user include dirs (for include "...")
;; This should be done via some automated mechanism for each project
;;(add-to-list 'company-c-headers-path-user "/development/fancy_project")

;; Deliberately not combining (company-capf with company-yasnippet) by using (())
;; This makes it very hard to reach snippets from within other completions

(add-hook 'c-mode-common-hook
		  (lambda ()
			(set (make-local-variable 'company-backends)
				 (setq company-backends (append '((:separate company-capf :separate  company-yasnippet) company-yasnippet) company-backends)))))

(provide 'init-c-cpp)

;;; init-c-cpp.el ends here
