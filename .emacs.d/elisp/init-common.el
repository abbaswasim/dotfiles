;;; init-common.el --- provides common usefull functions;
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'json)
(require 'flycheck)

(defvar project-copyright-header)
(defvar project-namespace-name)

(setq project-copyright-header "// Copyright 2018")
(setq project-namespace-name "core_engine")

;; TODO: fix the interactive mode
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (interactive list)
  (while list
	(print (car list))
	(setq list (cdr list))))

(defvar clang-dbname)
(defvar clang-build-path)
(defvar first-part)
(defvar second-part)

(setq clang-dbname "compile_commands.json")
(setq clang-build-path nil)
(setq first-part "\(\(c++-mode \(flycheck-clang-include-path . \(")
(setq second-part "\)\)\)\)\n")

(defun flycheck-clang-get-json ()
  "Get the compile commands from `clang-dbname'."
  (let ((commands (json-read-file (expand-file-name clang-dbname clang-build-path))))
	(mapcar (lambda (item)
			  (cdr (assq 'command item)))
			commands)))

(defun flycheck-clang-get-compile-command (json)
  "Return the compile command for a given `JSON' fragment from the compile database."
  (mapcar (lambda (item)
			(let ((raw-cmds (split-string item)))
			  (seq-filter (lambda (it)
							(cond
							 ((string-match "-I" it) t)
							 (t nil)))
						  raw-cmds))
			)json))

(defun flycheck-clang-find-compiledb-dir (file-or-dir)
  "Given FILE-OR-DIR search up for `clang-dbname'.
Return the directory which contains the database or nil."
  (let ((root-dir
		 (locate-dominating-file
		  file-or-dir
		  clang-dbname)))
	(when root-dir
	  (expand-file-name root-dir))))

(defun clean-I-from-path (include-paths)
  "This function will remove the `-I' from the INCLUDE-PATHS."
  (mapcar (lambda (single-path)
			(let ((clean-path (substring single-path 2)))
			  (if (not (file-name-absolute-p clean-path))
				  (concat clang-build-path (replace-regexp-in-string "^[.\/]+" "/" clean-path))
				(file-truename clean-path))))
		  include-paths))

(defun flatten (list)
  "This function will flatten LIST.
If input is ((\"a\") (\"b\" \"c\")) it will output (\"a\" \"b\" \"c\")"
  (mapcan (lambda (x) (if (listp x) x nil)) list))

(defun my-add-headers-include-to-dir-local (cl-build-path file-name)
  "This function load compile_commands.json relative to CL-BUILD-PATH FILE-NAME.

  It then parses it for all the headers"
  (let ((commands (flycheck-clang-get-json)))
	(if commands
		(delete-dups (flatten (flycheck-clang-get-compile-command commands))))))

;; fix this function and add comments
(defun create-dir-locals-el-at-root ()
  "This function create .dir-locals.el at the root of the project.
This will be used for flycheck headers files"
  (interactive)
  (unless clang-build-path
	(setq clang-build-path (flycheck-clang-find-compiledb-dir buffer-file-name)))
  (if clang-build-path
	(with-temp-file (concat clang-build-path "/.dir-locals.el" )
	  (insert first-part)
	  (mapc (lambda (item)
			  (insert "\"")
			  (insert item)
			  (insert "\" "))
			(clean-I-from-path (my-add-headers-include-to-dir-local clang-build-path buffer-file-name)))
	  (insert second-part))))

(defun toggle-maximize-buffer ()
  "Maximize current buffer."
 (interactive)
 (if (= 1 (length (window-list)))
	 (jump-to-register '_)
   (progn
	 (window-configuration-to-register '_)
	 (delete-other-windows))))

(setq help-window-select t)

(defun describe-thing-in-popup ()
  "Show full documentation of the symbol at point."
  (interactive)
  (let* ((thing (symbol-at-point))
			   (description (describe-function thing)))
	(quit-window)
	(popup-tip description
			   :point (point)
			   :around t
			   :height 100
			   :scroll-bar t
			   :margin t)))

(global-set-key (kbd "s-5") 'describe-thing-in-popup)

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
  (find-file (concat name ".inl"))
  (insert project-copyright-header)
  (insert-namespace project-namespace-name)
  (save-buffer)
  (find-file (concat name ".hpp"))
  (insert project-copyright-header)
  (insert "\n#pragma once")
  (insert-namespace project-namespace-name)
  (insert-header-include name ".inl")
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

(provide 'init-common)
;;; init-common.el ends here
