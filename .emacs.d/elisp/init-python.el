;;; init-python --- initialize cmake
;;; Commentary:

;;; Code:

;; enable lsp-mode for python , if completion doesn't work make sure to 'pip install jedi' and 'pip install 'python-lsp-server[all]''

(add-hook 'python-mode-hook 'lsp)

(provide 'init-python)

;;; init-python.el ends here
