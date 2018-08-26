;; init-common.el --- provides common usefull functions
;;; Commentary:

;;; Code:

(require 'json)
(require 'flycheck)

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

(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.
BUFFER may be either a buffer or its name (a string)."
  (interactive)
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
	  (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
		(when (kill-buffer buffer)      ; Only delete windows if buffer killed.
		  (dolist (win  wins)           ; (User might keep buffer if modified.)
			(when (window-live-p win)
			  ;; Ignore error, in particular,
			  ;; "Attempt to delete the sole visible or iconified frame".
			  (condition-case nil (delete-window win) (error nil))))))))

(defun describe-thing-in-popup ()
  "Show full documentation of the symbol at point."
  (interactive)
  (let* ((thing (symbol-at-point))
			   (description (describe-function thing)))
	(kill-buffer-and-its-windows "*Help*")
	(popup-tip description
			   :point (point)
			   :around t
			   :height 50
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

(provide 'init-common)
;;; init-common.el ends here
