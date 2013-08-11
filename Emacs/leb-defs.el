; leb-defs.el
;
; This file contains various definitions that are used by functions and
; a user's .emacs file.
;
; beegle  27-Aug-2008  Created generic file.
;

(defvar leb-emacs-automode (cons "\\.emacs.*\\'" 'emacs-lisp-mode))
(defvar leb-cshrc-automode (cons "\\.cshrc.*" 'sh-mode))
(defvar leb-bashrc-automode (cons "\\.bash.*" 'sh-mode))
(defvar leb-dxx-automode (cons "\\*\\.dxx" 'c++-mode))

; CVS/ClearCase named files.
(defvar leb-cvscc-automode 
  (cons "\\.\\([CHch]\\|cc\\|hh\\)\\(@@\\)?\\(\<.*\>\\)?.*\\'" 'c++-mode))

(defvar leb-cvscc-c-automode
  (cons "\\.[ch]\\(@@\\)?\\(\<.*\>\\)?.*\\'" 'c-mode))

(defvar leb-pre-post-condition "@precondition \n\n@postcondition \n\n")

; Make the module available to the world.
(provide 'leb-defs)

