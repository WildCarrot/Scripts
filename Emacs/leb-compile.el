;
; Functions for building via emacs.
;

(defvar leb-sandbox-root "~"
  "Root of the sandbox to use in paths as retrieved from the environment.")

(defvar leb-build-variant "x86_64"
  "The default variant to build.")

(defvar leb-build-variant-full "x86_64"
  "The full variant to build.")

(defvar leb-build-variant-test "test.x86_64"
  "The variant to build the tests.")

(defvar leb-make-command "" "The basic make command.")
(defvar leb-make-full-command "" "The make command to make a usable image.")
(defvar leb-log-command "" "The basic error log show command.")

(defconst build-make "make ")
(defconst build-log  "log -e ")
; This is really only useful when building must be invoked on a machine
; other than the one Emacs is running on.
(defconst leb-ssh "ssh -Y -i /u/beegle/.ssh/id_rsa host ")

;; Setup the basic make commands.
(defun leb-set-compile-cmds ()
  "Setup the make commands based on the values of `leb-sandbox-root`."
  (interactive)
  (setq leb-make-command (concat leb-ssh "\""
				 "cd " leb-sandbox-root " && " build-make
				 leb-build-variant
				 "\""))
  (setq leb-make-full-command (concat leb-ssh "\""
				      "cd " leb-sandbox-root "&& " build-make
				      leb-build-variant-full
				      "\""))
  (setq leb-log-command (concat leb-ssh "\""
				"cd " leb-sandbox-root "&& " build-log
				"\""))
)

(defun na-build-make (variant)
  (interactive "sBuild Variant: ")
  (if (not (eq buffer-file-name nil))
      (save-excursion
	(let* ((currpwd (file-name-directory buffer-file-truename))
	       (buildbuffer "*buildout*"))
	(set-buffer (get-buffer-create buildbuffer))
	(erase-buffer)
	(shell-command
	 (concat
	  "(PWD=" currpwd
	  " build make " variant
	  " && "
	  "PWD=" currpwd
	  " build log -e ) &")
	 buildbuffer)))))


;;; Compile commands for use in key bindings
(defun leb-compile (commands)
  (interactive "sMake arguments: ")
  (compile (concat leb-ssh "\""
		   "cd " leb-sandbox-root " && " build-make " " commands
		   "\"")))

(defun leb-view-log ()
  (interactive)
  (if (not (eq buffer-file-name nil))
      (save-excursion
	(let* ((currpwd (file-name-directory buffer-file-truename))
	       (buildbuffer "*buildout*"))
	  (set-buffer (get-buffer-create buildbuffer))
	  (setq default-directory (concat leb-sandbox-root "/")); "/../build"))
	  (compilation-mode)
	  (erase-buffer)
	  (shell-command (concat leb-ssh "\""
				 "cd " (concat leb-sandbox-root "/"); "/../build") 
				 " && " build-log
				 "\"")
			 buildbuffer))
      )
  )
)

(defun leb-compile-this-file (file)
  (interactive "i")
  (leb-compile (concat (file-name-sans-extension 
                        (file-name-nondirectory buffer-file-name))
                       ".o")))

(defun leb-compile-build (commands)
  (interactive "sbuild make: ")
  (leb-compile commands)
)

(defun leb-compile-default () 
  (interactive) 
  (leb-compile leb-build-variant)
)
(defun leb-compile-full () 
  (interactive) 
  (leb-compile leb-build-variant-full)
)
(defun leb-compile-clean  (commands) (interactive "i") (leb-compile "clean"))

(defun leb-compile-test () 
  (interactive) 
  (leb-compile leb-build-variant-test)
)

(defun leb-target-sim () (interactive) 
  (setq leb-build-variant "sim")
  (setq leb-build-variant-full "sim")
)

(defun leb-target-hw () (interactive)
  (setq leb-build-variant "x86")
  (setq leb-build-variant-full "x86")
)

(defun leb-target-hw64 () (interactive)
  (setq leb-build-variant "x86_64")
  (setq leb-build-variant-full "x86_64")
)

;; Get the list of tasks for the menu
;(defun leb-task-menu-filter (menu)
;  "leb task menu filter--returns menu list of [task (leb-set-task task) t]..."
;  (append (mapcar (lambda (task)
;		    (vector task
;			    (list 'leb-set-task task) 't
;                            ;:style toggle
;                            ;':selected (string-equal 'leb-task task)
;			    ))
;		  (list "secsrv" "secui" "cfgsrv" "almui" "cfgui" "isclib" "fml"))
;	  menu)
;  )


;;; Add an item to the Menu bar with the compile commands in it
;(defun leb-setup-compile-menu (arg)
;  (interactive "i")
;  (setq leb-compile-menu
;        (list "Compiles"
;              '("Set Task" :filter leb-task-menu-filter "-" "other")
;              ;(vector "foo" '(leb-set-task) ":style toggle :selected (string-equal \"foo\" \"foo\")")
;;              (vector (concat "Set Task (" leb-task ")")
;;                      '(leb-set-task) 't)
;;                      '(mapcar (lambda (fn) (eval fn)) 
;;                               (list '(leb-set-task leb-task))) 't)
;                                     ;(leb-setup-compile-menu arg))) 't)
;              ["Make all" leb-compile-no-ask t]
;              ["Make this file" leb-compile-this-file t]
;              ["Rebootstrap" leb-bootstrap t]
;              ["Make clean" leb-compile-clean t]
;              "-"
;              ["Alpha" leb-set-alpha-build 
;               :style toggle
;               :selected (string-equal leb-compile-platform "alpha")]
;              ["Linux" leb-set-linux-build 
;               :style toggle
;               :selected (string-equal leb-compile-platform "linux")]
;              ))
;  (add-submenu nil leb-compile-menu))

;;; Add key bindings to make things easy to run
(defun leb-setup-compile-keys ()
  "Sets up some function keys to run compile commands.  They are F2 through
F5 as follows:
  F2 = Run the compile command on the default variant
  F3 = Run the view log command
  F4 = Run the compile command on the full variant
  F5 = Interactively run the compile command (for passing specific variants)"
  (interactive)
  (global-set-key [f2] 'leb-compile-default)
  (global-set-key [kp-f2] 'leb-compile-default)
  (global-set-key [f3] 'leb-view-log)
  (global-set-key [kp-f3] 'leb-view-log)
  (global-set-key [f4] 'leb-compile-full)
  (global-set-key [kp-f4] 'leb-compile-full)
  (global-set-key [f5] 'leb-compile)
  (global-set-key [kp-f5] 'leb-compile)
)

;; Make these functions available to the world
(provide 'leb-compile)
