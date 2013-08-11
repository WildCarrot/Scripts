; Share and Enjoy.
;

; Looks
(setq font-lock-maximum-decoration t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq visible-bell t) ; flash screen instead of beeping
(setq screen-title-format "%S: %f")
(set-background-color "#000000")
(set-foreground-color "#f0d030")

; I want the matching paren to be highlighted.
(condition-case nil
    (let ()
      (require 'paren)
      (paren-set-mode 'paren)
      )
  (error nil))

; Save my place: buffers open, where scrolled to, etc.
(if (featurep 'xemacs)
    (let ()
      (require 'saveconf)
      (setq auto-save-and-recover-context t)
      (setq save-buffer-context t)
      (if (null (cdr command-line-args))
	  (setq inhibit-startup-message (recover-context)))
      (setq bookmark-save-flag 1)
    )
  (let ()
    (setq desktop-save-mode 1)
  )
)

; Make the fonts bigger.
; The size setting doesn't seem to work on Mac, but the font change does.
(defun leb-enlarge-fonts ()
  (interactive)
  (custom-set-faces
 '(default ((t (:family "Anonymous Pro" :height 120))) t)
; '(font-lock-comment-face ((t (:family "Anonymous Pro" :foreground "darkred" :size "14pt"))) t))
; '(font-lock-keyword-face ((t (:foreground "purple"))) t)
; '(default ((t (:family "Bitstream Vera Sans Mono" :size "12pt"))) t)
; '(font-lock-comment-face ((t (:family "Bitstream Vera Sans Mono" :foreground "darkred" :size "12pt"))) t))
; '(font-lock-keyword-face ((t (:foreground "purple"))) t)
  )
)
(leb-enlarge-fonts)

; Make the cursor visible and get rid of the useless toolbars.
(if (not (featurep 'xemacs))
    (let ()
      (set-cursor-color "red")
      ; Cygwin doesn't like this command.
      (if (not (eq system-type 'cygwin))
	  (tool-bar-mode -1)
      )
    )
)
(if (featurep 'xemacs)
  (set-specifier top-toolbar-height (cons (selected-frame) 0))
)

; Highlight trailing whitespace.
(require 'whitespace)
(setq whitespace-style (quote (face trailing newline)))
(global-whitespace-mode 1)

; Include my scripts.
(setq load-path
      (append (list
	       "~/Scripts/Emacs/"
;              "/other/paths/here"
	      )
	      load-path
       )
)


; Load a useful terminal emulator.
(load "term/vt300")

; Tweak some xemacs settings.
(if (featurep 'xemacs)
    (let ()
      ; Allow the ability to highlight and replace text (or delete it).
      (require 'pending-del)
      (turn-on-pending-delete)

      ; Mini-buffer auto-completion.
      (load-library "completer")
      (setq minibuffer-max-depth nil)

      ; Resize the mini-buffer as necessary.
      (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
      (resize-minibuffer-mode)
      (setq resize-minibuffer-window-exactly nil)

      ; Fancy mini-buffer behavior when typing filenames.
      (require 'default-dir)
    )
)

; Set the Greeting/Scratch message.
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

; Don't add new lines with the down-cursor
(setq next-line-add-newlines nil)

; Set automatic mode detection for some common file extensions.
(load "leb-defs")
(require 'leb-defs)

;(defalias 'perl-mode 'cperl-mode)

(load "sensitive")

(setq auto-mode-alist
      (append
       (list
	leb-emacs-automode
	leb-cshrc-automode
	leb-bashrc-automode
	leb-dxx-automode
	'("\\.gpg$" . sensitive)
	;leb-cvscc-automode
	;leb-cvscc-c-automode
	;'("\\*.c" . c-mode)
	)
       auto-mode-alist))

; Kind of heavy-handed, but emacs was being picky about which files
; it would fontify (ie, none of them happened automatically).
(turn-on-font-lock)


; Setup mode hooks.

(defun leb-perl-mode-hook ()
  "Coding standards for Perl."
  (cperl-indent-level 4)
  (cperl-brace-offset 0)
  (cperl-close-paren-offset -4)
  (cperl-continued-statement-offset 4)
  (cperl-continued-brace-offset 0)
  (cperl-indent-parens-as-block t)
  (cperl-tab-always-indent t)
  (indent-tabs-mode nil)
)
;(setq leb-debug-msg-type "STAT_LOG")
(require 'leb-cc)

(add-hook 'c-mode-common-hook 'leb-c-mode-common-hook)
(add-hook 'c-mode-hook 'leb-c-mode-hook)
(add-hook 'c++-mode-hook 'leb-c++-mode-hook)
(add-hook 'cperl-mode-common-hook 'leb-perl-mode-hook)


; Revert without query
(setq revert-without-query '("")) ; all files

; Get shortcuts and utils for code and tests.
(require 'leb-goto-other)
;(require 'simple-p4)
(require 'leb-text-formatting)

(global-set-key "\M-2" 'center-line)
(global-set-key "\C-x\C-o" 'leb-goto-other)
(global-set-key "\C-x\C-p" 'leb-goto-other-window)
(global-set-key "\C-x\C-m" 'leb-memo-to-docpp)
(global-set-key "\C-x\C-j" 'goto-line)
(global-set-key "\C-x-" 'leb-underline-heading)
(global-set-key "\C-xf" 'find-file)
(global-set-key "\C-xy" 'revert-buffer)
(global-set-key "\C-x\C-y" 'revert-buffer)
(global-set-key "\C-xw" 'leb-debug-region)
(global-set-key "\M-s" 'leb-insert-debug-msg)
(global-set-key "\C-x\C-c" nil) ; Don't accidentally exit.
(global-set-key "\C-cexit" 'save-buffers-kill-emacs)
(global-set-key "\C-z" nil) ; Don't accidentally minimize the window.
(global-set-key "\M-." 'gtags-find-tag)
(global-set-key "\M-," 'gtags-find-rtag)
(global-set-key "\M-?" 'gtags-find-symbol)
(global-set-key "\C-cut" 'leb-insert-ut-error)
(global-set-key "\C-c\C-c" 'comment-region)

(if (featurep 'xemacs)
    (let ()
      ; GNU Emacs doesn't like the single quote for
      ; the function key.  I didn't feel like figuring
      ; out what the emacs-agnostic key binding is.
      (global-set-key 'f9  'leb-check-out)
      (global-set-key 'f10 'leb-un-check-out)
      ;(global-set-key 'f11 'leb-check-in)
      (global-set-key [mouse-4] 'scroll-down)
      (global-set-key [mouse-5] 'scroll-up)
    )
)


(if (not (featurep 'xemacs))
    (let ()
      (desktop-read)
    )
)


;; (defun lsco ()
;;   (interactive)
;;   (p4-opened)
;; )

;; (defun leb-setv (defdir)
;;   (interactive "sDefault directory: ")
;;   "Set the default-directory to the root of a sandbox."
;;   (setq default-directory (concat defdir "/")); "subdir/i/work/in/"))
;;   (setq leb-sandbox-root (concat defdir "/")); "subdir/i/build/from/"))
;;   (leb-set-compile-cmds)
;;   (leb-setup-compile-keys)
;;   (leb-target-hw)
;; )

;; (defun leb-reload-sandboxes ()
;;   (interactive)
;;   (load-library "sandboxes.el")
;; )

;; (leb-reload-sandboxes)

;; (defun leb-find-change-for-line ()
;;   (interactive)
;;   (leb-find-change (buffer-substring (line-beginning-position)
;;				     (line-end-position)))
;; )

;; (if (featurep 'xemacs)
;;     (let ()
;;       ; GNU Emacs doesn't like the single quote for
;;       ; the function key.  I didn't feel like figuring
;;       ; out what the emacs-agnostic key binding is.
;;       (global-set-key 'f12 'leb-find-change-for-line)
;;     )
;; )

; CEDET & ECB junk
;(load-file "~/Emacs/cedet-1.0pre6/common/cedet.el")
;(semantic-load-enable-minimum-features)
;(semantic-load-enable-code-helpers)
;(require 'ecb-autoloads)
;(require 'semantic-ia)


;; ;; Enable using cscope with Xemacs
;; ;(if (featurep 'xemacs)
;; ;    (let ()
;;       (require 'xcscope)
;;       (if (string-equal system-type "darwin")
;;	  (setq cscope-program "/opt/local/bin/cscope")
;;	(if (string-equal (system-name) "beegle-lxp2.local")
;;	    (setq cscope-program "/opt/local/bin/cscope")
;;	  (setq cscope-program "/usr/software/rats/bin/cscope-15.6")))
;;       (setq cscope-use-relative-paths t)
;;       (setq cscope-do-not-update-database t)
;;       (set-face-foreground 'cscope-line-face "white") ; This works on black background emacs...
;; ;      )
;; ;)

; Org mode
;; (if (featurep 'xemacs)
;;     (let ()
;;       (require 'org-install)
;;       (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;       (global-set-key "\C-cV" 'org-show-entry)
;;       (setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "MOVED(m!)")))
;;       (setq org-log-done 'time)
;;       (setq org-tag-faces
;;	    '(("goal" . (:foreground "blue"))
;;	      ("admin" . (:bold t))
;;	      ("testing". "green")
;;	      ("imp" . org-todo)))
;;       (load-library "projects.el")
;;     )
;; )

; For MobileOrg
;; Set to the location of your Org files on your local system
;(setq org-directory "/home/beegle/org")
;; Set to the name of the file where new notes will be stored
;(setq org-mobile-inbox-for-pull "/home/beegle/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;(setq org-mobile-directory "/home/beegle/My Dropbox/MobileOrg")

; Remember within Org mode
;(org-remember-insinuate)
;(setq org-directory "/u/beegle/Projects/")
;(setq org-default-notes-file (concat org-directory "/notes.org"))
;(define-key global-map "\C-cr" 'org-remember)

