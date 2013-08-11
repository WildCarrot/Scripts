; leb-cc.el
;
; This file contains functions (optionally) used when editing C and C++ files.
;
; beegle  27-Aug-2008  Created generic file.
; beegle  09-Mar-2011  Added copyright update function.
;

(require 'leb-utils)
(require 'cc-mode)

(defun leb-c-mode-hook ()
  "LEB rules to run when going to C mode."
  (c-set-style "bsd")
  (setq c-default-style "bsd")
  (setq indent-tabs-mode nil)
  ; Uses tabs, not spaces.
  (setq tab-width 8
      indent-tabs-mode 1
      tab-stop-list 
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
)

(defun leb-c++-mode-hook ()
  "LEB rules to run when going to C++ mode."
  (c-set-style "stroustrup")
  (setq c-default-style "stroustrup")
  ; Align like ctor()
  ;                : foo()
  ;                , bar()
  (c-set-offset 'member-init-cont '-2)
)

(defun leb-c-mode-common-hook ()
  "LEB rules to run when going to C or C++ mode.  This is the same for both."
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'block-close 0)
  (c-set-offset 'arglist-close 0) ; lone closing paren on a line for arglist
;;  (leb-set-c-shl-bindings)
)

(defun leb-update-copyright ()
  "Update the copyright statement in the file."
  (interactive)
  (let ((beg (point))
        (end nil))
    (save-excursion
      (let ()
        (beginning-of-buffer)
        (search-forward "Copyright (c) ")
        (search-forward " ") ; Skip the numbers.
        (backward-char)
        (insert "-2013")
        )
      )
    )
)

;; (defvar leb-c-mode-prefix t)

;; (defun leb-set-c-shl-bindings ()
;;    "Set some useful c-mode key bindings."
;;    (interactive)
;;   (leb-set-prefix-mode-map (current-local-map) '(control ?c)
;; 			   '((el . leb-c-ifthenelse)
;; 			     (if . leb-c-simple-if)
;; 			     (wh . leb-c-whileloop)
;; 			     (for . leb-c-forloop)
;; 			     (stat . leb-insert-debug-msg)
;; 			     )
;; 			   'leb-c-mode-prefix)
;; )

(defun leb-c-insert (string &optional forward-word forward-char goto-end)
  "Inserts and indents the string."
  (leb-insert string t forward-word nil forward-char goto-end)
  )
(defun leb-c-forloop (&optional arg)
  (interactive "p")
  (leb-c-insert "for (;;)\n{\n}\n" 1 2 arg)
  )
(defun leb-c-whileloop (&optional arg)
  (interactive "p")
  (leb-c-insert "while ()\n{\n}\n" 1 2 arg)
  )
(defun leb-c-simple-if (&optional arg)
  (interactive "p")
  (leb-c-insert "if ()\n{\n}\n" 1 2 arg)
  )
(defun leb-c-ifthenelse (&optional arg)
  (interactive "p")
  (leb-c-insert "if ()\n{\n}\nelse\n{\n}\n" 1 2 arg)
  )

; If the caller doesn't define it, default to STAT_INFO in stat_msg calls.
(if (not (boundp 'leb-debug-msg-type))
    (setq leb-debug-msg-type "STAT_INFO")
)

(defun leb-insert-debug-msg ()
  "Insert a debug-msg call."
  (interactive)
  (leb-c-insert "dbg_printf(\"\\n\");\n")
  (beginning-of-line)
  (search-forward "\"")
  )


; My personal functions
(defun leb-debug-region ()
  "Put #warnings around debug code so it can be found easily."
  (interactive)
  (let ((orig-point (point-marker)))
    (if (region-active-p)
	(let ((beg (region-beginning))
	      (end (region-end))
	      (warnstart "#warning \"BEEGLE - DEBUG\"\n")
	      (warnend   "#warning \"BEEGLE - END DEBUG\"\n"))
	  (goto-char beg)
	  (insert warnstart)
	  (goto-char (+ end (length warnstart)))
	  (insert warnend)
	  (goto-char (marker-position orig-point))
	  (next-line 1)
	)
      (message "No active region.")
    )
  )
)

(defun leb-c++-forlist (&optional list-type list-name itr-name itr-type)
  "Insert a C++ list iterator, optionally over the given list variable."
  (interactive)
  (when (not list-type) (setq list-type "list<>"))
  (when (not list-name) (setq list-name "items"))
  (when (not itr-name)  (setq itr-name "itr"))
  (when (not itr-type)  (setq itr-type "const_iterator"))
  (leb-c-insert (concat "for (" list-type "::" itr-type " " itr-name
			" = " list-name ".begin();\n"
			itr-name " != " list-name ".end();\n"
			"++" itr-name ")\n{\n\n}\n"))
  (search-forward "{\n")
  (c-indent-line)
)

(defun leb-foreach-list ()
  "Break up the most recent kill buffer as a declaration of a list to
   iterate over."
  (interactive)
  (let ((var-list (reverse (leb-split-string (current-kill 0 t) ? )))
	(var-name "")
	(var-type ""))
    (setq var-name (car var-list))
    (if (string-equal (substring var-name 0 1) "&")
	(setq var-name (substring var-name 1)))
    (setq var-type (join-string (reverse (cdr var-list)) " "))
    (leb-c++-forlist var-type var-name)
    )
)

; Support for the "function" menu, listing all functions in a file.
(defun leb-function-menu-hook ()
  "Hook to customize local function menu handling."
  (setq fume-max-items 25
	fume-fn-window-position 3
	fume-auto-position-popup t
	fume-display-in-modeline-p t
	fume-menubar-menu-location nil
	fume-buffer-name "*Function List*"
	fume-no-prompt-on-valid-default nil)
  (fume-add-menubar-entry)
  )

(defun leb-function-menu ()
  "C/C++ source code function/methods menu setup."
  (add-hook 'find-file-hooks 'leb-function-menu-hook)
  (add-hook 'br-mode-hook 'leb-function-menu-hook)
  (define-key global-map "\C-cl" 'fume-list-functions)
  (define-key global-map "\C-cg" 'fume-prompt-function-goto)
  (fume-defvar-local fume-display-in-modeline-p t)
  )

(defun leb-memo-to-doc (&optional docpp-style)
  (interactive)
  "Convert a \"/// @memo\" line to an @doc style comment."
  (let ((comment "[ \t]*///[ \t]*")
        (doc "/** "))
    (when docpp-style 
      (if (= docpp-style 1) 
          (let ()
            (setq comment (concat comment "@memo[ \t]+"))
            (setq doc (concat doc "\n    @doc \n    "))
            )
        )
      )
    (search-forward-regexp comment)
    (replace-match doc)
    (next-line 1)
    (beginning-of-line)
    (insert "*/\n")
    (c-indent-line)
    )
)

(defun leb-memo-to-docpp ()
  "Default to using doc++ style memo-to-doc."
  (interactive)
  (leb-memo-to-doc 1)
)


(defun leb-cleanup-arg-name (argname)
  "Remove all * or & from an argument name."
  (replace-in-string (replace-in-string argname "\*" "") "\&" "")
)

(defun leb-add-param-tags ()
  (interactive)
  "Convert method/function arguments to @param tags.  This assumes that
a comment block exists for this function, ending with */.  This function
will also position the point after the first @param tag."
  (search-forward-regexp "[A-Za-z0-9_\+=<>\!]+\(")
  (let ((beg (point))
	(end nil)
	(argstr "")
	(args ()))
    (search-forward-regexp "\)")
    (forward-char -1)
    (setq end (point))

    ; Prepare the insert area.
    (search-backward-regexp "\*/")

    ; Get the argument string.
    (setq argstr (buffer-substring beg end))

    ; Parse the argument string.
    (if (not (string-equal argstr ""))
	(setq args (leb-split-string argstr ?,))
      )
    (while (not (null args))
      (let ((words ()))
	(open-line 1)
	(setq words (leb-split-string 
		     (leb-collapse-whitespace (car args)) ? ))
	(leb-c-insert (concat "@param " 
			      (leb-cleanup-arg-name (car (reverse words))) 
			      "    "))
	(next-line 1)
	)
      (setq args (cdr args))
      )

    ; Set the point after the first @param.
    (if (not (string-equal argstr ""))
	(let ()
	  (search-backward-regexp "@doc")
	  (search-forward-regexp "@param .+    ")
	  (end-of-line)
	  )
      )
    )
)

(defun leb-insert-ut-error ()
  "Insert a warning into unit-test files."
  (interactive)
  (insert "#ifndef UNITTEST_BUILD")
  (newline)
  (insert "#error \"MUST ONLY BE USED IN UNITTEST BUILDS.\"")
  (newline)
  (insert "#endif")
  (newline)
  (newline)
)

; Make this module available to the world.
(provide 'leb-cc)
