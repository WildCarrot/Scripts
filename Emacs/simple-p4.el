; simple-p4.el
;
; This file contains various functions for simple P4 interaction.
;
; beegle  27-Aug-2008  Initial creation.
; beegle  23-Aug-2010  Added use of local script for finding a change number.
;

;(defun leb-check-out (comment)
;  "Check out the current file from source control."
;  (interactive "sCheck out comment: ")
;  (message (shell-command-to-string 
;            (concat "px edit -c \"" comment "\" " buffer-file-name)))
;  (revert-buffer)
;)
(defconst simple-p4-ssh "ssh -i /u/beegle/.ssh/id_rsa host.example.com ")
(defconst simple-p4-buffer "*simple-p4*")

(defun leb-check-out ()
  "Check out the current file from source control."
  (interactive)
  (message (shell-command-to-string 
	    (concat simple-p4-ssh "\""
		    "cd " (file-name-directory (buffer-file-name))
		    "; p4 edit " (file-name-nondirectory (buffer-file-name))
		    "\""
             )))
  (revert-buffer)
)

(defun leb-un-check-out ()
  "Undo check out of the current file."
  (interactive)
  (message (shell-command-to-string 
            (concat simple-p4-ssh "\""
		    "cd " (file-name-directory (buffer-file-name))
		    "; cp " (file-name-nondirectory (buffer-file-name))
		    (concat (file-name-nondirectory (buffer-file-name)) ".keep")
		    "; p4 revert " (file-name-nondirectory (buffer-file-name))
		    "\""
             )))
  (revert-buffer)
)

(defun leb-check-in (comment)
  "Check the current file into source control with same comment."
  (interactive "i")
  (message (shell-command-to-string
            (concat simple-p4-ssh "\""
		    "cd " (file-name-directory (buffer-file-name))
		    "; p4 submit " (file-name-nondirectory (buffer-file-name))
		    "\""
             )))
  (revert-buffer)
)

(defun leb-find-change (line-text)
  "Find the change number for a given line in the current buffer's file."
  (interactive "i")
  (message "Finding change number...")
  (shell-command
	    (concat simple-p4-ssh "\""
		    "cd " (file-name-directory (buffer-file-name))
		    "; /u/beegle/bin/scripts/find_edit -f " 
		    (file-name-nondirectory (buffer-file-name))
		    " -a '" line-text "'\""
	     ) simple-p4-buffer)
  (message "Done!")
)


; Make the module available to the world.
(provide 'simple-p4)

