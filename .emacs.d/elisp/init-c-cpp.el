;;; init-c-cpp --- initialize cmake
;;; Commentary:

;;; Code:

;; enable c++-mode in .h instead of c-mode and other associations
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.mesh.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.task.glsl\\'" . glsl-mode))

;; Objective-c and Objective-C++ mode enable
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'auto-mode-alist '("\\.gltf\\'" . javascript-mode))

;; Autoload code folding minor mode
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'glsl-mode-hook 'turn-on-font-lock)

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

(add-hook 'glsl-mode-hook
		  (lambda ()
			(set (make-local-variable 'company-backends)
				 (add-to-list 'company-backends '(company-glsl :with company-dabbrev :with company-dabbrev-code :with company-capf)))))

(provide 'init-c-cpp)

;;; init-c-cpp.el ends here
