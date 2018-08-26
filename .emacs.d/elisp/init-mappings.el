;;; init-mappings.el -- all my global mappings goes here
;;; Commentary:
;;; Code:
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "s-f") 'helm-occur)

(global-set-key (kbd "s-z") 'helm-resume)

;; show all symbols in current buffer
(global-set-key (kbd "s-2") 'rtags-imenu)

(global-set-key (kbd "<f5>") 'toggle-maximize-buffer)

(global-set-key (kbd "s-=") 'hs-toggle-hiding)

(global-set-key (kbd "s-v") 'evil-paste-after)
;; remap C-j back to eval-print-last-sexp it is hijacked by (electric-newline-and-maybe-indent)
(global-set-key (kbd "C-j") 'electric-newline-and-maybe-indent)

(provide 'init-mappings)
;;; init-mappings.el ends here
