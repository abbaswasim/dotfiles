;;; package --- summary
;;; Commentary:
;;; init-dap.el dap-mode settings
;;; Code:

(require 'dap-mode)

;; Enable dap-mode and define its settings
(require 'dap-lldb)

;; -*- lexical-binding: t -*-
;; From https://emacs-lsp.github.io/dap-mode/page/how-to/

;; (define-minor-mode dap-running-session-mode
;;   "A mode for adding keybindings to running sessions"
;;   nil
;;   nil
;;   (make-sparse-keymap)
;;   (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
;;   ;; The following code adds to the dap-terminated-hook
;;   ;; so that this minor mode will be deactivated when the debugger finishes
;;   (when dap-running-session-mode
;;	(let ((session-at-creation (dap--cur-active-session-or-die)))
;;	  (add-hook 'dap-terminated-hook
;;				(lambda (session)
;;				  (when (eq session session-at-creation)
;;					(dap-running-session-mode -1)))))))

;; ;; Activate this minor mode when dap is initialized
;; (add-hook 'dap-session-created-hook 'dap-running-session-mode)

;; ;; Activate this minor mode when hitting a breakpoint in another file
;; (add-hook 'dap-stopped-hook 'dap-running-session-mode)

;; ;; Activate this minor mode when stepping into code in another file
;; (add-hook 'dap-stack-frame-changed-hook (lambda (session)
;;					  (when (dap--session-running session)
;;						(dap-running-session-mode 1))))

(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

(define-minor-mode dap-running-session-mode
  "A mode for adding keybindings to running sessions"
  :init-value nil
  :lighter " DAP-DBG"
  :keymap my-mode-map
  (when (bound-and-true-p evil-mode)
	(evil-normalize-keymaps))  ; if you use evil, this is necessary to update the keymaps
  )

;; Activate this minor mode when dap is initialized
(add-hook 'dap-session-created-hook #'dap-running-session-mode)
;; Activate this minor mode when hitting a breakpoint in another file
(add-hook 'dap-stopped-hook #'dap-running-session-mode)
;; Activate this minor mode when stepping into code in another file
;; (add-hook 'dap-stack-frame-changed-hook (lambda (session)
;;										  (when (dap--session-running session)
;;											(dap-running-session-mode 1))))

(add-hook 'dap-stack-frame-changed-hook (lambda (session)
										  (dap-running-session-mode 1)))

(add-hook 'dap-terminated-hook (lambda (session)
								 (dap-running-session-mode -1)))

(require 'general)

;; Define evil key bindings for this special dap-mode minor mode
(general-define-key
 :states 'normal
 :keymaps 'my-mode-map
 "n" 'dap-next
 "s" 'dap-step-in
 "o" 'dap-step-out
 "c" 'dap-continue
 "b" 'dap-breakpoint-toggle
 )

(defun clean-dap-mode ()
  (interactive)
  (dap-delete-all-sessions)
  (dap-mode -1)
  (dap-ui-mode -1)
  (dap-ui-controls-mode -1))

(defun start-dap-debug ()
  (interactive)
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode)
  (dap-ui-controls-mode 1)
  (dap-ui-sessions)
  (dap-ui-locals)
  (dap-ui-breakpoints)
  (dap-ui-repl)
  (dap-debug))

(evil-leader/set-key "0" 'clean-dap-mode)

(general-define-key
 :states 'normal
 "<f18>" 'start-dap-debug)

(provide 'init-dap)

;;; init-dap.el ends here
