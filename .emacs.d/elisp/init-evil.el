;;; init-evil.el -- Evil mode configuration.
;;; Commentary:
;;; Code:

(require 'evil)
(evil-mode 1)

;; Change single character modes to full names
(setq evil-normal-state-tag (propertize " NORMAL "))
(setq evil-emacs-state-tag (propertize " EMACS "))
(setq evil-insert-state-tag (propertize " INSERT "))
(setq evil-motion-state-tag (propertize " MOTION "))
(setq evil-visual-state-tag (propertize " VISUAL "))
(setq evil-operator-state-tag (propertize " OPERATOR "))

;; whether replaying commands like (.) moves the cursor or leaves it where it is
(setq evil-repeat-move-cursor nil)

;; undo changes at a finner level
(setq evil-want-fine-undo 'fine)

;; when getting out of insert mode don't move cursor back one character
(setq evil-move-cursor-back nil)

;; leave search highlighted like vim
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; bind C-s to go half-screen up, paired with C-d going halfscreen down
(define-key evil-normal-state-map (kbd "C-s") (lambda () (interactive) (evil-scroll-up nil)))

;; Move to visual lines instead of actual lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Ex-command mode is triggered by ; instead of :
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map (kbd "s-q") 'evil-quit)

;; enable and setup other evil packages as well
(require 'evil-leader)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Add all my leader mappings
(require 'init-leader)

(require 'evil-surround)
(global-evil-surround-mode 1)

;; use evil-mc for multiple cursors (C-n/C-p) to create cursors for current word
;; gru to remove all cursors
(require 'evil-mc)
(global-evil-mc-mode  1)

;; Some mappings for tabs and buffers
;; (load "elscreen" "ElScreen" t)
;; (elscreen-start)
;; (define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
;; (define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
;; (define-key evil-normal-state-map "gb" 'elscreen-previous) ;previous tab
;; (define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
;; (setq elscreen-display-tab nil)

(define-key evil-normal-state-map "go" 'other-window)

;; flycheck shortcuts for evil
(define-key evil-normal-state-map "g8" 'flycheck-next-error)
(define-key evil-normal-state-map "g7" 'flycheck-previous-error)

;; Evil numbers shortcuts
(define-key evil-normal-state-map "gt" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "gy" 'evil-numbers/dec-at-pt)

;; Override gf to use helm-projectile-find-file-dwim instead
(define-key evil-normal-state-map "gf" 'helm-projectile-find-file-dwim)

;; :tabnew and the likes
;; (global-evil-tabs-mode t)

(setq evil-emacs-state-cursor '("#3A2E58" box))
(setq evil-normal-state-cursor '("#789000" box))
(setq evil-visual-state-cursor '("#EDBA00" box))
(setq evil-insert-state-cursor '("#487BB4" bar))
(setq evil-replace-state-cursor '("#CF352F" hollow))
(setq evil-operator-state-cursor '("#429FA5" hollow))

;; Make escape quit everything, whenever possible.
(defun minibuffer-keyboard-quit ()
	"Abort recursive edit.
	In Delete Selection mode, if the mark is active, just deactivate it;
	then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
	  (setq deactivate-mark  t)
	  (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
	  (abort-recursive-edit)))

(defun undo-all-cursors-with-quit()
  (interactive)
  (evil-mc-undo-all-cursors)
  (keyboard-quit))

(define-key evil-normal-state-map [escape] 'undo-all-cursors-with-quit)
(define-key evil-visual-state-map [escape] 'undo-all-cursors-with-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(evil-define-motion evil-goto-definition-current-doc ()
  "Go to definition or first occurrence of symbol under point in current document only."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
		 (search (format "\\_<%s\\_>" (regexp-quote string))))
	(if (null search)
		(user-error "No symbol under cursor")
	  (setq isearch-forward t)
	  (cond (t (evil-search search t t (point-min)))))))

(define-key evil-motion-state-map "gh" 'evil-goto-definition-current-doc)

(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)

;; Disable Evil mode in certain modes
(add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
(add-to-list 'evil-emacs-state-modes 'debugger-mode)

(provide 'init-evil)
;;; init-evil ends here
