(when (memq window-system '(mac ns))
;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
  (setq ns-use-srgb-colorspace t)) ;; This should be set to nil for Emacs 28 and onwards

(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'wave)

(require 'powerline-evil)

(defface my-pl-segment1-active
  '((t (:foreground "#4D4D4D" :background "#E1B61A")))
  "Powerline first segment active face.")

(defface my-pl-segment1-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline first segment inactive face.")

(defface my-pl-segment2-active
  '((t (:foreground "#F5E39F" :background "#8A7119")))
  "Powerline second segment active face.")

(defface my-pl-segment2-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline second segment inactive face.")

(defface my-pl-segment3-active
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline third segment active face.")

(defface my-pl-segment3-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline third segment inactive face.")

(defun air--powerline-default-theme ()
  "Set up my custom Powerline with Evil indicators."
  (interactive)
  (setq-default mode-line-format
				'("%e"
				  (:eval
				   (let* ((active (powerline-selected-window-active))
						  (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
						  (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
						  (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
						  (separator-left (intern (format "powerline-%s-%s"
														  (powerline-current-separator)
														  (car powerline-default-separator-dir))))
						  (separator-right (intern (format "powerline-%s-%s"
														   (powerline-current-separator)
														   (cdr powerline-default-separator-dir))))
						  (lhs (list (let ((evil-face (powerline-evil-face)))
									   (if evil-mode
										   (powerline-raw (powerline-evil-tag) evil-face)
										 ))
									 (if evil-mode
										 (funcall separator-left (powerline-evil-face) seg1))
									 (powerline-raw "%b" seg1 'l)
									 (powerline-raw "[%*]" seg1 'l)
									 (when (and (boundp 'which-func-mode) which-func-mode)
									   (powerline-raw which-func-format seg1 'l))
									 (powerline-raw " " seg1)
									 (funcall separator-left seg1 seg2)
									 (when (boundp 'erc-modified-channels-object)
									   (powerline-raw erc-modified-channels-object seg2 'l))
									 (powerline-major-mode seg2 'l)
									 (powerline-process seg2)
									 (powerline-minor-modes seg2 'l)
									 (powerline-narrow seg2 'l)
									 (powerline-raw " " seg2)
									 (funcall separator-left seg2 seg3)
									 (powerline-vc seg3 'r)
									 (when (bound-and-true-p nyan-mode)
									   (powerline-raw (list (nyan-create)) seg3 'l))))
						  (rhs (list (powerline-raw global-mode-string seg3 'r)
									 (funcall separator-right seg3 seg2)
									 (unless window-system
									   (powerline-raw (char-to-string #xe0a1) seg2 'l))
									 (powerline-raw "%4l" seg2 'l)
									 (powerline-raw ":" seg2 'l)
									 (powerline-raw "%3c" seg2 'r)
									 (funcall separator-right seg2 seg1)
									 (powerline-raw " " seg1)
									 (powerline-raw "%6p" seg1 'r)
									 (when powerline-display-hud
									   (powerline-hud seg1 seg3)))))
					 (concat (powerline-render lhs)
							 (powerline-fill seg3 (powerline-width rhs))
							 (powerline-render rhs)))))))

(air--powerline-default-theme)

(provide 'init-powerline)
