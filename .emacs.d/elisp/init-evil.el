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
(define-key evil-normal-state-map "gt" 'evil-numbers/inc-at-pt-incremental)
(define-key evil-normal-state-map "gy" 'evil-numbers/dec-at-pt-incremental)

;; Override gf to use helm-projectile-find-file-dwim instead
(define-key evil-normal-state-map "gf" 'helm-projectile-find-file-dwim)

;; Remap gw to swap characters
(evil-define-key 'normal 'global "gw" "xphh")

(defun copy-and-past-immidiately ()
  "Copy selection and past afterwards.  BEG and END."
  (interactive)
  (let* ((original-column (current-column))
		 (original-line (line-number-at-pos)))
	(evil-yank (region-beginning) (region-end))
	(goto-line original-line)
	(move-to-column original-column)
	(evil-open-below 0)
	(evil-normal-state)
	(evil-paste-after 1)))

;; Compund key to copy past a block in one go
(evil-define-key 'normal 'global "gc" 'copy-and-past-immidiately)

(defun next-and-center ()
  "Move to next search item and center."
  (interactive)
  (progn
	(evil-ex-search-next 1)
	(evil-scroll-line-to-center nil)))

(defun previous-and-center ()
  "Move to previous search item and center."
  (interactive)
  (progn
	(evil-ex-search-previous 1)
	(evil-scroll-line-to-center nil)))

;; Better to center search results
(evil-define-key 'normal 'global "n" 'next-and-center)
(evil-define-key 'normal 'global "N" 'previous-and-center)

(defun swap-buffers-accross()
  "Swaps the buffers between the 2-windows that I always use"
  (interactive)
  (other-window 1)
  (window-swap-states))

;; Lets swap buffers between two windows
(evil-define-key 'normal 'global "gj" 'swap-buffers-accross)

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

(define-key evil-motion-state-map "gh" "gd*N")

;; Very important setting that makes searching very useful
(setq-default evil-symbol-word-search t)

(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)

(define-key evil-normal-state-map ",cy" 'evilnc-yank-and-comment-operator)

;; Disable Evil mode in certain modes
(add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
(add-to-list 'evil-emacs-state-modes 'debugger-mode)

(provide 'init-evil)
;;; init-evil ends here
