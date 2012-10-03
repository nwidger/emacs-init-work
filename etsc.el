;; Time-stamp: <26 Sep 2012 at 14:01:49 by niels on lark.lan>

;; Make sure you have the following scripts somewhere in your PATH:
;; maketags, findfiles, list_images

;; REQUIRE

(require 'ansi-color)
(require 'anything-config)
(require 'cedet-cscope)
(require 'ede)
(provide 'ede/locate)
(require 'ffap)
(require 'grep)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'term)
(require 'xcscope)

;; GLOBALS

(setq etsc-project-file "cscope.files")
(setq etsc-project-dirs '())

;; PROJECT DIRECTORIES

(defun etsc-add-project-dir (dir)
  "Add directory DIR to `etsc-project-dirs'."
  (interactive "fProject directory: ")
    (add-to-list 'etsc-project-dirs
		 (file-name-directory (expand-file-name dir)) t)
    (etsc-set-ibuffer-saved-filter-groups)
    etsc-project-dirs)

(defun etsc-remove-project-dir (dir)
  "Remove directory DIR from `etsc-project-dirs'."
  (interactive "fProject directory: ")
  (setq etsc-project-dirs
	(delete (file-name-directory (expand-file-name dir)) etsc-project-dirs))
  (etsc-set-ibuffer-saved-filter-groups)
  etsc-project-dirs)

;; ISWITCHB SUPPORT

(defun ede-iswitchb (arg)
  "When ARG is nil, simply run `iswitchb'.  With ARG, use
`iswitchb' to switch to a buffer in current EDE project matching
a substring.  Displays error message and does nothing if current
buffer is not associated with an EDE project."
  (interactive "P")
  (if (not arg)
      (iswitchb)
    (let* ((proj (ede-current-project))
	   (iswitchb-make-buflist-hook
	    (lambda ()
	      (setq iswitchb-temp-buflist
		    (delq nil (mapcar
			       (lambda (x)
				 (let* ((f (buffer-local-value 'buffer-file-name (get-buffer x)))
					(p (cond (f (ede-current-project (file-name-directory f))))))
				   (cond ((and proj p (eq proj p)) x)))) 
			       iswitchb-temp-buflist))))))
      (if (not proj)
	  (message "Not in a project!")
	(iswitchb)))))

;; IBUFFER SUPPORT

(define-ibuffer-filter ede-project
    "Toggle current view to buffers with EDE project root
directory matching QUALIFIER."
  (:description "ede-project"
		:reader (read-from-minibuffer "Filter by EDE project root directory (regexp): "))
  (let* ((file (buffer-local-value 'buffer-file-name buf))
	 (proj (cond (file (ede-current-project (file-name-directory file)))))
	 (root (cond (proj (ede-project-root-directory proj)))))
    (when root
      (string-match qualifier root))))

(define-ibuffer-sorter ede-project
  "Sort buffers by EDE project root directory."
  (:description "ede-project")
  (let* ((fa (buffer-local-value 'buffer-file-name (car a)))
	 (pa (cond (fa (ede-current-project (file-name-directory fa)))))
	 (ra (cond (pa (ede-project-root-directory pa))))
	 (ba (buffer-name (car a)))
	 (fb (buffer-local-value 'buffer-file-name (car b)))
	 (pb (cond (fb (ede-current-project (file-name-directory fb)))))
	 (rb (cond (pb (ede-project-root-directory pb))))
	 (bb (buffer-name (car b))))
    (cond ((and ra (not rb)) t)
	  ((and (not ra) rb) nil)
	  ((and (not ra) (not rb)) (string< ba bb))
	  ((and ra rb) (cond ((string= ra rb) (string< ba bb)) (string< ra rb))))))

;; this is used by `define-ibuffer-column' below
(defun etsc-ibuffer-column-summarizer (list)
  "Return summary string indicating number of unique EDE project
root directories in LIST"
  (interactive)
  (let ((l (length (delete-if (lambda (a) (or (not a) (string-match "^[ \t]*$" a))) 
			      (delete-dups list)))))
    (format "%d %s" l (cond ((= l 1) "project") "projects"))))

(define-ibuffer-column ede-project
  (:name "EDE Project" :inline t :summarizer etsc-ibuffer-column-summarizer)
  (let* ((file (buffer-local-value 'buffer-file-name buffer))
	 (proj (cond (file (ede-current-project (file-name-directory file)))))
	 (root (cond (proj (ede-project-root-directory proj)))))
    (if (not root) "" root)))

;; example `ibuffer-formats' value.  The second column configuration
;; replaces filename-and-process with ede-project
(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 18 18 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " " filename-and-process)
	(mark modified read-only " "
	      (name 18 18 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " " ede-project)
	(mark " "
	      (name 16 -1)
	      " " filename)))

(defun ibuffer-mark-by-ede-project-regexp (regexp)
  "Mark all buffers whose EDE project root directory matches
REGEXP."
  (interactive "sMark by EDE project root directory (regexp): ")
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (let* ((file (buffer-local-value 'buffer-file-name buf))
	      (proj (cond (file (ede-current-project (file-name-directory file)))))
	      (root (cond (proj (ede-project-root-directory proj)))))
	 (when root
	   (string-match regexp root))))))

(defun etsc-set-ibuffer-saved-filter-groups ()
  "Generate filter group for each project in `etsc-project-dirs'
and store in `ibuffer-saved-filter-groups."
  (interactive)
  (let ((dirs etsc-project-dirs) (group '("etsc-projects")) d x)
    (while dirs
      (setq d (car dirs))
      (setq x (cons ede-project d))
      (add-to-list 'group (list d x) t)
      (setq dirs (cdr dirs)))
    (setq ibuffer-saved-filter-groups (list group))))

;; load saved filter groups generated by
;; `etsc-set-ibuffer-saved-filter-groups' when loading Ibuffer
(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "etsc-projects")))

;; example keybindings for the filter/mark/sort commands
(define-key ibuffer-mode-map (kbd "/ e") 'ibuffer-filter-by-ede-project)
(define-key ibuffer-mode-map (kbd "% e") 'ibuffer-mark-by-ede-project-regexp)
(define-key ibuffer-mode-map (kbd "s e") 'ibuffer-do-sort-by-ede-project)

;; ANYTHING SUPPORT

(defun etsc-project-file-to-anything-source (pf)
  "Return `anything' source for files in project file PF."
  (interactive "fGlobal links file: ")
  (let ((source '()) (files '()) (f nil))
    (with-temp-buffer
      (if (file-exists-p pf)
	  (insert-file-contents pf))
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	  (setq f (thing-at-point 'line))
	(setq f (replace-regexp-in-string "^[ \t]*" "" f))
	(setq f (replace-regexp-in-string "[ \t]*\r?\n$" "" f))
	(add-to-list 'files f)
	(forward-line 1)))
    (add-to-list 'source '(name . "Global links") t)
    (add-to-list 'source (cons 'candidates files) t)
    (add-to-list 'source '(type . file) t)))

(defun anything-for-etsc-project-file ()
  "Preconfigured `anything' source for opening files in current
project."
  (interactive)
  (let (root proj cmd pf)
    (setq proj (ede-current-project))
    (setq root (cond (proj (ede-project-root-directory proj))))
    (when root
      (setq pf (format "%s%s" root etsc-project-file)))
    (if (or (not pf) (not (file-exists-p pf)))
	(message "Could not determine project file!" etsc-project-file)
      (message "Building completions list...")
      (anything-other-buffer (etsc-project-file-to-anything-source pf)
			     "*anything-select-buffer-name*"))))

;; TERM

;; make backspace send ^H in ansi-term
(defun term-send-backspace  () (interactive) (term-send-raw-string "\C-H"))

;; GREP

(grep-apply-setting 'grep-command "grep EXPR -Hn -r --include=*.{tcl,c,cxx,h} --exclude-dir=\".svn\" ../..")

;; COMPILATION

(defun etsc-compile-command (&optional root)
  "Return a default `compile-command' string for project with
root directory ROOT or current project root directory if ROOT is
nil"
  (let (proj cmd)
       (when (not root)
	 (setq proj (ede-current-project))
	 (setq root (cond (proj (ede-project-root-directory proj)))))
       (if root
	   (setq cmd (format "cd %sbuild; make modules-cdrouter-mp -s V=0" root)))
       cmd))

(defun etsc-compilation-buffer-name-function (name-of-mode)
  "Return a buffer name to be used by `compile' for the current
project"
  (let* ((proj (ede-current-project))
	 (root (ede-project-root-directory proj))
	 (buf (cond (root (format "*%s compilation*" root)) ("*compilation*"))))
    buf))

;; SNIPPETS

(defun etsc-qasnippets ()
  (interactive)
  "Update snippets for current project."
  (let (cmd root (proj (ede-current-project)))
    (if (not proj)
	(message "Not in a project!")
      (setq root (ede-project-root-directory proj))
      (setq cmd (format "qasnippets %s ~/.emacs.d/snippets" root))
      (if (interactive-p)
          (setq cmd (read-input "Command: " cmd)))
      (unless (string-match "&[ \t]*\\'" cmd)
        (setq cmd (concat cmd " &")))
      (shell-command cmd (format "*%s qasnippets*" root)))))

;; ETAGS/CSCOPE

(defun etsc-maketags (&optional arg)
  (interactive "P")
  "Update etags and cscope database for current project.  With
prefix argument, also regenerate project file."
  (let (cmd root (proj (ede-current-project)))
    (if (not proj)
	(message "Not in a project!")
      (setq root (ede-project-root-directory proj))
      (if (not arg)
	  (setq cmd (format "cd %s; maketags -tc" root))
	(setq cmd (format "cd %s; findfiles; maketags -tc" root)))
      (if (interactive-p)
          (setq cmd (read-input "Command: " cmd)))
      ;; (unless (string-match "&[ \t]*\\'" cmd)
      ;;   (setq cmd (concat cmd " &")))
      (shell-command cmd (format "*%s maketags*" root))
      (message "maketags complete"))))

(defun etsc-cscope-database-regexps (&optional root)
  "Return a default `cscope-database-regexp' list for project
with root directory ROOT or current project root directory if
ROOT is nil"
  (let (proj regexps pf)
    (when (not root)
      (setq proj (ede-current-project))
      (setq root (cond (proj (ede-project-root-directory proj)))))
    (when root
      (setq pf (format "%s%s" root etsc-project-file))
      (setq regexps (list
		     (list (format "^%s.*" root)
			   '(t) ; use normal hier db search
			   (list pf '("-d"))
			   )
		     )))
    regexps))

;; EDE

(defun etsc-locate-fcn (name dir)
  "Find file NAME in project with directory root DIR"
  (message "(etsc-locate-fcn \"%s\" \"%s\")" name dir)
  (let ((pf (format "%s%s" (file-name-directory dir) etsc-project-file)))
    (if (not (file-exists-p pf))
	nil
      (let ((ret (shell-command-to-string (format "egrep '/%s$' %s" name pf))))
	(if (string= "" ret)
	    nil
	  (replace-regexp-in-string "[ \t]*\r?\n$" "" ret))))))

(defun etsc-proj-root-fcn (&optional dir)
  (message "(etsc-proj-root-fcn \"%s\")" (or dir default-directory))
  "Return the root fcn for DIR or `default-directory' if DIR is nil"
  (setq dir (or dir default-directory))
  (let (root (pfs etsc-project-dirs))
    (while (and pfs (not root))
      (let* (ret (pf (format "%s%s" (file-name-directory
				     (expand-file-name (car pfs)))
			     etsc-project-file)))
	(when (file-exists-p pf)
	  (setq ret (shell-command-to-string
		     (format "egrep '%s[^/]*$' %s" (expand-file-name dir) pf)))
	  (if (or (string= dir (file-name-directory pf)) (not (string= "" ret)))
	      (setq root (file-name-directory pf))))
	(setq pfs (cdr pfs))))
    root))

(defun etsc-proj-file-fcn (&optional dir)
  (message "(etsc-proj-file-fcn \"%s\")" (or dir "nil"))
  "Return a full file name to the project file stored in DIR."
  (if (not dir)
      etsc-project-file
    (let ((root (etsc-proj-root-fcn dir)) pf)
      (if (not root)
	  nil
	(setq pf (format "%s%s" (file-name-directory root) etsc-project-file))
	(if (not (file-exists-p pf))
	    nil
	  pf)))))

(defun etsc-project-filelist (file)
  "Generate list of files from project file FILE."
  (interactive "fProject file: ")
  (let ((files '()) (f nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	(setq f (thing-at-point 'line))
	(setq f (replace-regexp-in-string "^[ \t]*" "" f))
	(setq f (replace-regexp-in-string "[ \t]*\r?\n$" "" f))
	(add-to-list 'files f)
	(forward-line 1)))
    files))

(defun etsc-project-targets (pf)
  "Generate a list of `ede-cpp-root-target' from project file
PF."
  (let ((resize-mini-windows nil) (max-mini-window-height 0)
	(targets '()) (d nil) (target nil))
    (with-temp-buffer
      (shell-command (format "cat %s | /bin/sed -e 's/\\/[^/]*$/\\//' | /usr/bin/sort -u" pf) (current-buffer))
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	(setq d (thing-at-point 'line))
	(setq d (replace-regexp-in-string "^[ \t]*" "" d))
	(setq d (replace-regexp-in-string "[ \t]*\r?\n$" "" d))
	(setq target (etsc-project-target-dir d pf))
	(if target
	    (add-to-list 'targets target))
	(forward-line 1)))
    targets))

(defun etsc-project-rescan-targets ()
  "Rescan targets for current project"
  (interactive)
  (let ((proj (ede-current-project)) root pf)
    (if (not proj)
	(message "Not in a project!")
      (setq root (ede-project-root-directory proj))
      (setq pf (format "%s%s" root etsc-project-file))
      (if (file-exists-p pf)
	  (oset proj targets (etsc-project-targets pf))))))

(defun etsc-project-reparse-files (&optional root)
  "Reparse all files in project with root directory ROOT, or
current project if ROOT is nil."
  (interactive)
  (let (proj pf args)
    (when (not root)
      (setq proj (ede-current-project))
      (setq root (cond (proj (ede-project-root-directory proj)))))
    (setq pf (cond (root (format "%s%s" root "cscope.files"))))
    (if (or (not pf) (not (file-exists-p pf)))
	(message "Could not determine valid project file!")
      (setq args (etsc-project-filelist pf))
      (while args
	(princ (concat "Loading " (car args) "... "))
	(save-window-excursion
	  (let* ((buffer (find-file-noselect (car args)))
		 (tags nil))
	    (set-buffer buffer)
	    (setq tags (semantic-fetch-tags))
	    (princ (length tags))
	    (princ " tags found .\n"))
	  (setq args (cdr args))))
      (semanticdb-save-all-db))))

(defun etsc-project-target-dir (dir pf)
  "Generate `ede-cpp-root-target' for DIR in project with project
file PF."
  (let ((resize-mini-windows nil) (max-mini-window-height 0)
	(files '()) (f nil))
    (when (file-directory-p dir)
      (setq qdir (replace-regexp-in-string "\+" "\\\\+" dir))
      (with-temp-buffer
	(shell-command (format "/bin/egrep '^%s[^/]+$' %s" (file-name-directory qdir) pf) (current-buffer))
	(goto-char (point-min))
	(while (not (eobp))
	  (beginning-of-line)
	  (setq f (thing-at-point 'line))
	  (setq f (replace-regexp-in-string "^[ \t]*" "" f))
	  (setq f (replace-regexp-in-string "[ \t]*\r?\n$" "" f))
	  (add-to-list 'files f)
	  (forward-line 1)))
      (ede-cpp-root-target dir :name dir :path dir :source files))))

(defun etsc-load-type-fcn (dir)
  (message "(etsc-load-type-fcn \"%s\")" dir)
  "Load a project of type `cpp-root' for the directory DIR.
Return nil if there isn't one."
  (let (proj (pf (format "%s%s" (file-name-directory dir) etsc-project-file)))
    (when (file-exists-p pf)
      (setq proj (ede-cpp-root-project dir
			    :file pf
			    ;; :targets (etsc-project-targets pf)
			    :locate-fcn 'etsc-locate-fcn
			    :local-variables (list (cons 'compile-command
							 (etsc-compile-command dir))
						   (cons 'cscope-database-regexps
							 (etsc-cscope-database-regexps dir))
						   (cons 'compilation-buffer-name-function
							 'etsc-compilation-buffer-name-function))
			    ;; :keybindings '(
			    ;; 		   (cons "D" . 'ede-debug-target)
			    ;; 		   (cons "R" . 'ede-run-target)
			    ;; 		   (cons "M" . 'etsc-maketags)
			    ;; 		   (cons "P" . 'etsc-puts)
			    ;; 		   )
			    ))
      (ede-enable-locate-on-project proj))
    proj))

(add-to-list 'ede-project-class-files
	     (ede-project-autoload "etsc"
				   :name "etsc"
				   :file 'ede/cpp-root
				   :proj-file 'etsc-proj-file-fcn
				   :proj-root 'etsc-proj-root-fcn
				   :load-type 'etsc-load-type-fcn
				   :class-sym 'ede-cpp-root-project)
	     )

(defun ede-find-file-at-point ()
  "Find FILE in project."
  (interactive)
  (ede-find-file (thing-at-point 'filename)))

;; HOOKS

(add-hook 'tcl-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-f") 'anything-for-etsc-project-file)
	     ;; (local-set-key (kbd "C-x b") 'ede-iswitchb)
	     (local-set-key (kbd "C-c . F") 'ede-find-file-at-point)))

(add-hook 'compilation-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "c") 'compile)))

;; support ANSI color sequences in compilation buffer
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; set speedbar face
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "Monospace-10")

(add-hook 'speedbar-mode-hook 
	  '(lambda () 
	     (buffer-face-set 'speedbar-face)))

(provide 'etsc)
