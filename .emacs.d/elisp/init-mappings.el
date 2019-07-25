;;; init-mappings.el -- all my global mappings goes here
;;; Commentary:
;;; Code:
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "s-f") 'helm-occur)

(global-set-key (kbd "s-z") 'helm-resume)

(global-set-key (kbd "<f5>") 'toggle-maximize-buffer)

(global-set-key (kbd "s-=") 'hs-toggle-hiding)

(global-set-key (kbd "s-v") 'evil-paste-after)

;; remap C-j back to eval-print-last-sexp it is hijacked by (electric-newline-and-maybe-indent)
(global-set-key (kbd "C-j") 'electric-newline-and-maybe-indent)

;; Quickly turn SetSomething to set_something
(global-set-key (kbd "s-6") 'string-inflection-underscore)

;; Format selected region with clang-format, or complete buffer
(global-set-key [C-s-tab] 'clang-format-region)
(global-set-key (kbd "s-7") 'clang-format-buffer)

(setq mac-command-modifier 'super) ; make cmd key do Meta
(setq mac-option-modifier 'meta) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
;; (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(provide 'init-mappings)
;;; init-mappings.el ends here
