; leb-goto-other.el
;
; This file contains functions for swapping between header and source files
; within the same directory, in the lib<->hdr/lib setup, and in the
; lib/master<->hdr/master/lib setup.
;
; beeglele  11-Aug-2004  Split here from dotemacs.el.
; beegle    2012-06-12   Added callers for super simple version.
;

; Get the leb-join-string and *Emacs agnostic split functions.
(require 'leb-utils)

; This is the extremely simple version that only looks in the current
; directory for the other file.
(defun leb-goto-other-fn (find-file-fn)
  "Go to the corresponding C or H file for the current buffer."
  (interactive "i")
  (let ((curr-file "")
	(curr-ext "")
	(other-ext "")
        (other-file-base ""))
    (setq curr-file (file-name-nondirectory (buffer-file-name)))
    (setq curr-ext (file-name-extension curr-file))
    (setq other-file-base (concat (file-name-sans-extension curr-file) "."))
    (if (string-equal curr-ext "h") 
        (let ()
          (setq other-ext "cc")
          (if (not (file-exists-p (concat other-file-base other-ext)))
              (setq other-ext "c"))
          ))
    (if (string-equal curr-ext "H")
	(setq other-ext "C"))
    (if (string-equal curr-ext "cc")
	(setq other-ext "h"))
    (if (string-equal curr-ext "c")
	(setq other-ext "h"))
    (if (string-equal curr-ext "C")
	(setq other-ext "H"))
    (funcall find-file-fn (concat other-file-base other-ext))
    )
)

(defun leb-goto-other ()
  "Go to the corresponding C or H file for the current buffer in the current window."
  (interactive)
  (leb-goto-other-fn 'find-file)
)

(defun leb-goto-other-window ()
  "Go to the corresponding C or H file for the current buffer in another window."
  (interactive)
  (goto-other-fn 'find-file-other-window)
)


; The rest of these functions are for the complicated version involving
; more path manipulation.

(defun leb-otherfile (filename one two)
  "Swap between the two given extensions."
  (let ((basename  (file-name-sans-extension 
		    (file-name-nondirectory filename)))
	(extension (file-name-extension (file-name-nondirectory filename))))
    (if (string-equal extension one)
	(concat basename "." two)
      (if (string-equal extension two)
	  (concat basename "." one)
	""
      )
    )
  )
)

(defun leb-other-exists (toccs path file)
  "Determine if a file with the given paths and filename exists."
  (let ((outpath ""))
    ; The games we play here with null and empty strings are to keep the
    ; path free of double slashes ("//").  If there are two slashes,
    ; emacs will name the buffer differently and the same file opened via
    ; this method will be "different" from the buffer for the file opened
    ; directly.
    (setq outpath (concat (leb-join-path toccs t)
			  (leb-join-path path)))
    (if (file-exists-p (concat outpath file))
	outpath
      ""
    )
  )
)

(defun leb-otherdir (otherfile toccspath ccspath flip-fn)
  "Apply the flip-fn to the ccspath and see if the otherfile exists."
  (let ((path "")
	(flipped ()))
    (setq flipped (funcall flip-fn ccspath))
    (setq path (leb-other-exists toccspath flipped otherfile))
    path
  )
)

(defun leb-goto-other-general (otherfile-fn-list location-fn-list open-fn)
  (interactive "i")
  "Switch to the other half of a pair of files.
The otherfile-fn-list is a list of functions to convert a given filename
to the other filename.
The location-fn-list is a list of functions to convert a given path
 (relative to ccs) to the other path.
The open-fn is the function to call on the final filename of the other file.
Defaults are provided for all of these via the leb-goto-other and
leb-goto-other-window functions."
  (let ((thisfile  (file-name-nondirectory (buffer-file-name)))
	(toccspath ())
	(ccspath   ())
        (otherfile "")
        (otherpath ""))

    ; Get the ccs relative path.
    (setq toccspath (car  (leb-ccspath (buffer-file-name))))
    (setq ccspath   (cadr (leb-ccspath (buffer-file-name))))

    ; Get the name of the otherfile.
    (while (and (string-equal otherfile "")
                (not (null otherfile-fn-list)))
      (let ()
       (setq otherfile (funcall (car otherfile-fn-list) thisfile))
       (setq otherfile-fn-list (cdr otherfile-fn-list))
       )
      )

    ; Get the location of the otherfile.
    (while (and (string-equal otherpath "")
		(not (null location-fn-list)))
      (let ()
	(setq otherpath (leb-otherdir otherfile toccspath ccspath 
				      (car location-fn-list)))
	(setq location-fn-list (cdr location-fn-list))
	)
      )

    ; If we have a valid location and otherfile name, call the open-fn.
    (if (and (not (string-equal otherfile ""))
	     (not (string-equal otherpath "")))
	(funcall open-fn (concat otherpath otherfile))
      (message "Could not determine other file or location.")
    )
  )
)

(setq leb-otherfile-defaults '( (lambda (x) (leb-otherfile x "C" "H"))
				(lambda (x) (leb-otherfile x "cc" "h"))
				(lambda (x) (leb-otherfile x "c" "h"))
				(lambda (x) (leb-otherfile x "cpp" "hpp")) ) )

(setq leb-loc-fn-defaults '(
			    ; "Convert" . -> .
			    (lambda (x) x)
			    ; Convert lib -> include.
			    (lambda (x) (list "include"))
			    ; Convert lib -> hdr/lib.
			    (lambda (x) (list "hdr" (car x)))
			    ; Convert hdr/lib -> lib.
			    (lambda (x) (list (cadr x)))
			    ; Convert common -> hdr.
			    (lambda (x) (list "hdr"))
			    ; Convert hdr -> common.
			    (lambda (x) (list "common"))
			    ; Convert lib/master -> hdr/master/lib.
			    (lambda (x) (list "hdr" (cadr x) (car x)))
			    ; Convert hdr/master/lib -> lib/master.
			    (lambda (x) (list (car (reverse x)) (cadr x)))
			    ; Convert hdr/project/lib -> lib.
			    (lambda (x) (list (car (reverse x))))
			    ; Convert lib -> hdr/project/lib.
			    (lambda (x) (list "hdr" "project" (car x)))
			    ; Convert lib -> hdr/master/lib.
			    (lambda (x) (list "hdr" "master" (car x)))
			    ; Convert common/master -> hdr/master.
			    (lambda (x) (list "hdr" (cadr x)))
			    ; Convert hdr/master -> common/master.
			    (lambda (x) (list "common" (cadr x)))
			    ) )

; These are the functions you should map keys to (if so inclined).
(defun leb-goto-other-window (&optional dummy)
  (interactive "i")
  "Switch to the other file in another window.  See leb-goto-other-general for
more details."
  (leb-goto-other-general leb-otherfile-defaults leb-loc-fn-defaults 
			  'find-file-other-window)
)

(defun leb-goto-other (&optional dummy)
  (interactive "i")
  "Switch to the other file.  See leb-goto-other-general for more details."
  (leb-goto-other-general leb-otherfile-defaults leb-loc-fn-defaults
			  'find-file)
)


; Make the module available to the world.
(provide 'leb-goto-other)
