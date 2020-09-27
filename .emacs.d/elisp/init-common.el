;;; init-common.el --- provides common usefull functions;
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'json)
;; (require 'flycheck)

(defvar project-copyright-header)
(defvar project-namespace-name)

(setq project-copyright-header "// Copyright 2018")
(setq project-namespace-name "core_engine")

(setq help-window-select t)

(defun toggle-maximize-buffer ()
  "Maximize current buffer."
  (interactive)
  (if (= 1 (length (window-list)))
	  (jump-to-register '_)
	(progn
	  (window-configuration-to-register '_)
	  (delete-other-windows))))

(defvar my-posframe-buffer "*my-describe-thingy-buffer*")

(defun remove-description-posframe ()
  "Remove any visible posframe created earlier."
  (posframe-hide-all)
  (remove-hook 'pre-command-hook #'remove-description-posframe))

(require 'posframe)

(defun describe-thing-in-popup ()
  "Show full documentation of the symbol at point."
  (interactive)
  (let* ((thing (symbol-at-point))
		 (description (describe-function thing)))
	(quit-window)
	(posframe-show
	 my-posframe-buffer
	 :string description
	 :foreground-color "#2d2d2d"
	 :background-color "#ffcc66"
	 :position (point))
	(add-hook 'pre-command-hook #'remove-description-posframe)))

 ;; in python mode show help in window
(evil-define-key 'normal python-mode-map (kbd "s-1") 'elpy-doc)
 ;; in elisp mode show help in popup
(evil-define-key 'normal emacs-lisp-mode-map (kbd "s-1") 'describe-thing-in-popup)
;; in cmake mode show help in side window
(evil-define-key 'normal cmake-mode-map (kbd "C-M-s-!") 'cmake-help)
;; keyboard macro for the above to press enter automatically
(evil-define-key 'normal cmake-mode-map (kbd "s-1") '(lambda (&optional arg)
													   "Keyboard macro."
													   (interactive "p")
													   (kmacro-exec-ring-item
														(quote
														 ([209715233 return] 0 "%d")) arg)))


(defvar notes-folder "/development/notes/")
(defvar notes-text "\n\n* Atendees:\n** Ours:\n** <other> \n\n* Notes:\n")

(defun create-note (name)
  "This create a note with NAME provided.
Always opens the file NAME in notes-folder."
  (interactive "sWhat do you want to call your note: ")
  (cd notes-folder)
  (find-file (concat name ".org"))
  (insert (concat name "\n"))
  (insert-char 61 (string-width name)) ;; inserts '=' name length many times
  (insert notes-text)
  (goto-char (+ (* 2 (string-width name)) 23)))

(global-set-key (kbd "s-4") 'create-note)

(defun insert-header-include (name extension)
  "Insert #include <NAME.EXTENSION> in place.
Make sure extension has '.' included."
  (insert "\n\n#include \"")
  (insert name)
  (insert extension)
  (insert "\""))

(defun insert-namespace (namespace)
  "Insert NAMESPACE in place."
  (insert "\n\nnamespace ")
  (insert namespace)
  (insert "\n{\n\n} // namespace ")
  (insert namespace))

(defun c++-create-header (name header-dir source-dir)
  "Create header/inline and source file with specificed NAME in HEADER-DIR and SOURCE-DIR."
  (interactive
   (let* ((X (read-string "What do you want to call your C/C++ source: "))
		  (Y (read-directory-name "Where do you want header/inline: " (projectile-project-root)))
		  (Z (read-directory-name "Where do you want source: " Y)))
	 (list X Y Z)))
  (cd source-dir)
  (find-file (concat name ".cpp"))
  (insert project-copyright-header)
  (insert-header-include name ".hpp")
  (insert-namespace project-namespace-name)
  (save-buffer)
  (cd header-dir)
  (find-file (concat name ".hh"))
  (insert project-copyright-header)
  (insert-namespace project-namespace-name)
  (save-buffer)
  (find-file (concat name ".hpp"))
  (insert project-copyright-header)
  (insert "\n#pragma once")
  (insert-namespace project-namespace-name)
  (insert-header-include name ".hh")
  (save-buffer))

(global-set-key (kbd "s-8") 'c++-create-header)

;; Let projectile save buffer on compile
(setq compilation-ask-about-save nil)

;; Run projectile compile command to start building the poroject
(defun compile-projectile-project-no-prompt ()
  "Compiles projectile project without prompting for compile command.

It uses projectile-project-compilation-cmd from .dir-locals.el if defined
Or run projectile-compile-command first to save a command in history"
  (interactive)
  (let ((compilation-read-command nil))
	(projectile-compile-project nil)))

(global-set-key (kbd "s-<return>") 'compile-projectile-project-no-prompt)

;; Cycles through the compilation errors only
(defun go-to-next-compile-error (&optional direction threshold)
  "Opens the next compiler error.

Sets `compilation-skip-threshold' to 2 to skip anything other than errors.
If `DIRECTION' is nil it moves to next error and -1 will take you to previous.
If `THRESHOLD' is 2 only traverses error, 1 means errors and warnings and 0 means everything."
  (interactive)
  (let ((compilation-skip-threshold threshold))
	(next-error direction)))

;; > to takes you to next error
;; < to takes you to previous error
(global-set-key (kbd "s-.") (lambda () (interactive) (go-to-next-compile-error 1 2)))
(global-set-key (kbd "s-,") (lambda () (interactive) (go-to-next-compile-error -1 2)))

;; > to takes you to next error+warning
;; < to takes you to previous error+warning
(global-set-key (kbd "s->") (lambda () (interactive) (go-to-next-compile-error 1 1)))
(global-set-key (kbd "s-<") (lambda () (interactive) (go-to-next-compile-error -1 1)))

;; > to takes you to next error+warning+info
;; < to takes you to previous error+warning+info
(global-set-key (kbd "C-s-.") (lambda () (interactive) (go-to-next-compile-error 1 0)))
(global-set-key (kbd "C-s-,") (lambda () (interactive) (go-to-next-compile-error -1 0)))

;; Always make compilation buffer open at the bootom of the windows at 40% of the height
(add-to-list 'display-buffer-alist
			 `("*compilation*"
			   (display-buffer-reuse-window
				display-buffer-in-side-window)
			   (reusable-frames . visible)
			   (side            . bottom)
			   (window-height   . 0.4)))


;; Make C++ function calls more prominent by assigning it a color
(font-lock-add-keywords 'c++-mode
						`((,(concat
							 "\\<[_a-zA-Z][_a-zA-Z0-9]*\\>"       ; Object identifier ;; Don't need this one just highlight anything that looks like function
							 "\\s *"                              ; Optional white space ;; Don't need this one just highlight anything that looks like function
							 "\\(?:\\.\\|->\\|::\\)"              ; Member access "(?:" in the regex means non-capturing group
							 "\\s *"                              ; Optional white space
							 "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
							 "\\s *"                              ; Optional white space
							 "(")                                 ; Paren for method invocation
						   1 'font-lock-function-name-face t)))

(font-lock-add-keywords
 'c++-mode
 '(
   ("\\<\\(\\sw+\\)?(" 1 font-lock-function-name-face) ;; Any free standing functions highlights
   ;; ("\\_<[A-Z]*_[A-Z_]+\\_>" . font-lock-doc-face) ;; For macros and CONSTANTS_CONSTANTS, doesn't fully work
   ("\\_<\\(\\+\\|-\\|0x\\)?\\([0-9]+\\)\\(\\.[0-9fulUL]+\\)?\\_>" . font-lock-warning-face) ; Number identifier
   ("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
   ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
   ("\\_<\\(FORCE_INLINE\\)" 1 font-lock-doc-face) ;; this broke again in emacs 28, but seems to be working now
   ("\\_<\\(ROAR_ENGINE_ITEM\\)" 1 font-lock-doc-face) ;; this broke again in emacs 28, but seems to be working now
   ))

;; If macros or CONSTANTS_CONSTANTS font-lock-face are added, enable this for it to work
(custom-set-variables '(c-noise-macro-names '("FORCE_INLINE" "ROAR_ENGINE_ITEM"))) ;; this broke again in emacs 28, but seems to be working now

(provide 'init-common)
;;; init-common.el ends here
