;;; package --- summary
;;; Commentary:
;;; init-neotree.el neotree-mode settings
(require 'neotree)
(require 'evil)

;;; Code:
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

(defvar neotree/previous-buffer nil)

(defun sublimetree-kill-buffer()
  "Kills neotree/previous-buffer if it exists."
  (interactive)
  (if neotree/previous-buffer
	   (if (get-buffer neotree/previous-buffer)
		   (kill-buffer neotree/previous-buffer)))
  (setq neotree/previous-buffer nil))

(defun sublimetree-next-line (&optional arg)
  "Quick look like sublime text.
ARG are the same as `neo-open-file'."
  (interactive "P")
  (neotree-next-line)
  (sublimetree-kill-buffer)
  (neotree-enter arg)
  (setq neotree/previous-buffer (buffer-name))
  (neo-global--select-window))

(defun sublimetree-previous-line (&optional arg)
  "Quick Look like NeoTree open event.
ARG are the same as `neo-open-file'."
  (interactive "P")
  (neotree-previous-line)
  (sublimetree-kill-buffer)
  (neotree-enter arg)
  (setq neotree/previous-buffer (buffer-name))
  (neo-global--select-window))

(defun override-neotree-toggle ()
  "Overrides neotree-toggle so we can save current buffer."
  (interactive)
  (sublimetree-kill-buffer)
  (neotree-toggle))

(evil-define-key 'normal neotree-mode-map (kbd "n") 'sublimetree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'sublimetree-previous-line)

(global-set-key (kbd "s-§") 'override-neotree-toggle)

(provide 'init-neotree)

;;; init-neotree.el ends here
