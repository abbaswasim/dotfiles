;;; init-cmake --- initialize cmake
;;; Commentary:

;;; Code:

(require 'company-cmake)
(require 'cmake-font-lock)

;; Better syntax highlighting in cmake files
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(add-hook 'cmake-mode-hook
		  (lambda ()
			(set (make-local-variable 'company-backends)
				 '(company-files company-cmake company-dabbrev-code company-dabbrev company-capf))))

(provide 'init-cmake)

;;; init-cmake.el ends here
