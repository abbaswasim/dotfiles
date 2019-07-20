;;; init-c-cpp --- initialize cmake
;;; Commentary:

;;; Code:

;; (require 'cl)

;; (add-hook 'c-mode-common-hook '())

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

(provide 'init-c-cpp)

;;; init-c-cpp.el ends here
