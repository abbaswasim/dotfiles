;;; init-leader.el --- all leader key mappings are defined here
;;; Commentary:
;;; Code:
;; evil-leader is loaded in init-evil.el

;; <leader>d creates cursor for all occurences of the current word
(evil-leader/set-key "d" 'evil-mc-make-all-cursors)

;; <leader>v selects all occurences of a variable and you can start changing it
(evil-leader/set-key "v" 'iedit-mode)

;; <leader>o opens a helm-find-files dialog
(evil-leader/set-key "o" 'helm-find-files)

;; <leader>hjklf are reserved for easymotion navigation
;; (setq ace-jump-word-mode-use-query-char nil)
(evil-leader/set-key "e" 'evil-ace-jump-word-mode)
(evil-leader/set-key "l" 'evil-ace-jump-line-mode)
(evil-leader/set-key "c" 'evil-ace-jump-char-mode)

;; <leader>b switch to buffer
(evil-leader/set-key "b" 'switch-to-buffer)

;; <leader>k kill buffer
(evil-leader/set-key "k" 'kill-buffer)

;; <leader>z resume previous helm-session
(evil-leader/set-key "z" 'helm-resume)

;; <leader>a show all symbols and can helm-search amongs them
;; (evil-leader/set-key "a" 'helm-gtags-select)

;; <leader>t toggle between header/source
(evil-leader/set-key "t" 'helm-projectile-find-other-file)

;; <leader>p helm-projectile
(evil-leader/set-key "p" 'helm-projectile)

;; <leader>w kill current buffer
(evil-leader/set-key "w" 'kill-this-buffer)

;; <leader>2 shows all symbols in current file to chose from
(evil-leader/set-key "2" 'helm-imenu)

;; <leader>f starts helm-projectile-ag at current symbol
(evil-leader/set-key "f" 'helm-projectile-ag)

(provide 'init-leader)
;;; init-leader.el ends here
