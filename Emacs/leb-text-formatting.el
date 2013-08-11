; Some functions for prettying plain text documents.
;

(defun leb-underline (&optional char)
  "Underline the current line from the beginning of the line to point."
  (interactive "cWhat character? ")
  (save-excursion
    (let ((num-unders (current-column))
	  (under-char ?-))
      ; If there is no line in the buffer below point, add a newline.
      (if (= 1 (forward-line 1))
	  (newline)
	)
      ; Use the given underline character, if given.
      (when char (setq under-char char))
      (insert-char under-char num-unders)
      )
    )
)

(defun leb-underline-heading ()
  "Underline the current line and position point under the new line of dashes.
   If the point is in column zero, underline the previous line."
  (interactive)
  (if (= 0 (current-column))
      (let ()
	(forward-line -1)
	(end-of-line)
	)
    (save-excursion
      (newline)
      )
    )
  (leb-underline)
  (forward-line 1) ; Get to dashes.
  (end-of-line)
  (newline) ; Go to the next line.
)

(provide 'leb-text-formatting)
