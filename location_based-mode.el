;=============================================================================
;
;  Program:   Location Based Edition
;
;  Copyright (C) 2010 by Benoit Leveau. All rights reserved.
;
;     This software is distributed WITHOUT ANY WARRANTY; without even
;     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;     PURPOSE.  See the above copyright notices for more information.
;
;=============================================================================
;;; location_based-mode.el --- minor-mode for customizations based on location

;------------------------------------------------------------------------------

;;; Commentary:

;------------------------------------------------------------------------------

;;; Code:

;------------------------------------------------------------------------------

(load-compile "buffer-bg.el")

;;
;; location_based mode startup function.
;;
(defun change-buffer-color ()
  (if (string-match "^\/software\/" (file-name-directory buffer-file-name))
	  (progn
		(buffer-bg-set-color "#683d8b" buffer-file-name)
		(if (not buffer-read-only)
			(toggle-read-only)))
	(if (or (string-match "^\/usr\/people\/[a-z]+\-[a-z]+\/tools" (file-name-directory buffer-file-name))
			(string-match "^~\/tools" (file-name-directory buffer-file-name)))
		(buffer-bg-set-color "#485d8b" buffer-file-name)
	  (buffer-bg-set-color nil buffer-file-name))))

(add-hook 'find-file-hook 'change-buffer-color)

;;; location_based-mode.el ends here
