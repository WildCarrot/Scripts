; leb-utils.el
;
; This file contains various functions that are used by other emacs functions.
;
; beegle  27-Aug-2008  Created generic version.
;

(defun leb-join-string (list sep)
  "Join a list of strings together with sep in between each item.
No leading or trailing separator is added."
  (cond ((null (cdr list)) (car list))
	(t (concat (car list)
		   sep
		   (leb-join-string (cdr list) sep))
	   )
	)
)

(defun leb-join-path (list &optional root sep)
  "Join a list of path parts together.  If no separator is specified,
a forward slash '/' is used.  Also, a leading separator is added if
root is t."
  (let ((pathstr "")
	(sepr    (if sep sep "/"))
	)
    (if root
	(setq pathstr sepr)
    )
    (while (not (null list))
      (let ()
	(if (not (string-equal (car list) ""))
	    (setq pathstr (concat pathstr (car list) sepr))
        )
	(setq list (cdr list))
      )
    )
    pathstr
  )
)

(defun leb-split-string (string char)
  "Does `split-string-by-char' in XEmacs and `split-string' in GNU/Emacs."
  (if (featurep 'xemacs)
       (split-string-by-char string char)  ; XEmacs
     (split-string string (char-to-string char)) ; GNU/Emacs
  )
)

(defun leb-collapse-whitespace (string)
  "Removes leading and trailing whitespace, collapses internal whitespace
down to a single space."
  (let ((str string)
	(chars ()))
    ; Replace non-space whitespace with a space.
    (setq str (replace-in-string str "\n" " "))
    (setq str (replace-in-string str "\t" " "))
    ; Replace sequences of many spaces with just one.
    (setq str (replace-in-string str "[ ]+" " "))

    (setq chars (string-to-list str))
    ; Remove leading space.
    (if (= (car chars) ? )
	(setq chars (cdr chars))
      )
    ; Remove trailing space.
    (setq chars (reverse chars))
    (if (= (car chars) ? )
	(setq chars (cdr chars))
      )
    (setq chars (reverse chars))
    (setq str (concat chars))

    ; Return the cleaned string.
    str
    )
)

(defun leb-insert (string
		   &optional indent forward-word forward-sexp forward-char
		   goto-end)
  "Inserts and indents the string."
  (let ((beg (point))
	(end nil))
    (save-excursion
      (let ()
	(insert string)
	(when indent (c-indent-region beg (point)))
	(setq end (point))
	)
      )
    (when  forward-word (forward-word forward-word))
    (when  forward-sexp (forward-sexp forward-sexp))
    (when  forward-char (forward-char forward-char))
    (when  goto-end     (goto-char end))
  )
)

(defun leb-prefix-region (prefix)
  "Add a prefix string to each line in the selection region."
  (interactive "sPrefix string: ")
  (if prefix
      (let ((count (count-lines (region-beginning) (region-end))))
        (goto-char (min (region-beginning) (region-end)))
        (while (> count 0)
          (setq count (1- count))
          (beginning-of-line 1)
          (insert prefix)
          (end-of-line 1)
          (forward-char 1))
	)
    )
)

(defvar leb-ccsroot "ccs")

(defun leb-ccspath (filename)
  "Get the given path down to a ccs relative path.
Return both the path upto and including ccs and the post-ccs path."
  (let ((ccspath (leb-split-string (file-name-directory filename) ?/))
	(toccs   ()))
    ; The next line will remove the trailing empty string element.
    (setq ccspath (reverse (cdr (reverse ccspath))))
    (while (and (not (string-equal (car ccspath) leb-ccsroot))
		(not (null ccspath)))
      (let ()
	(setq toccs   (append toccs (list (car ccspath))))
	(setq ccspath (cdr ccspath))
      )
    )
    ; Remove the left over ccs.
    (if (and (not (null ccspath))
	     (string-equal (car ccspath) leb-ccsroot))
	(let ()
	  (setq toccs   (append toccs (list (car ccspath))))
	  (setq ccspath (cdr ccspath))
	)
    )
    (list toccs ccspath)
  )
)

;; (defun leb-set-prefix-mode-map (mode-map mode-map-key-prefix mapping
;; 					 mode-map-prefix)
;;   "Setup the mode keybinding map, MODE-MAP. MAPPING is a list of (characters . function) cons pairs. The characters are prepended with the MODE-MAP-KEY-PREFIX and mapped to the associated function. The character may be specified as a symbol, string, or character. MODE-MAP-PREFIX map must be pre-defined via (define-prefix-command MODE-MAP-PREFIX t)." 
;;   (define-prefix-command mode-map-prefix t)
;;   (define-key mode-map (vector mode-map-key-prefix) mode-map-prefix)
;;   (mapc (lambda (character-function)
;; 	  (define-key mode-map
;; 	    (mapvector
;; 	     (lambda (x) x)
;; 	     (cons mode-map-key-prefix
;; 		   (let ((characters (car character-function)))
;; 		     (cond
;; 		      ((characterp characters) (list characters))
;; 		      ((stringp characters) (string-to-list characters))
;; 		      ((symbolp characters) (string-to-list
;; 					     (symbol-name characters)))
;; 		      )
;; 		     )
;; 		   ) ;cons
;; 	     ) ;mapvector
;; 	    (cdr character-function)))
;; 	mapping
;; 	)
;;   )

; Make the module available to the world.
(provide 'leb-utils)

