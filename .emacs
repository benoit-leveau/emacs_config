;;; .emacs.el --- Emacs Configuration File

;; Copyright (C) 2010  Benoit Leveau

;; Author: Benoit Leveau <benoit.leveau@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(message "Detecting computer configuration...")

;; Global switches
;;
(setq config-windows nil)
(setq config-linux nil)

(if (string-match "linux" system-configuration)
	(setq config-linux t)
  (if (string-match "windows" system-configuration)
	  (setq config-windows t)))

(message (concat "Computer '" system-name "' | " system-configuration))

;; directory to put various .el files into
;;
(if config-windows
	(setq custom-load-path "C:/emacs_includes")
  (setq custom-load-path "~/emacs_includes"))

(setq load-path (cons custom-load-path load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading (and compiling if necessary) any .el file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-compile (file_to_load &optional ignore_non_existing)
  "Load a compile version of the file (or compile it if necessary)"
  (interactive "sFile to load: ")
  (setq file (concat custom-load-path (concat "/" file_to_load)))
  (setq file-compiled (concat file "c"))
  
  ;; Make sure source file exists
  (if (not (file-exists-p file))
	  (if (not ignore_non_existing)
		  (message (concat "Can't load " (concat file_to_load ". Make sure path is correct!"))))
	;; Load compiled version of file (compile the .el file if necessary)
	(if (or (not (file-exists-p file-compiled)) (file-newer-than-file-p file file-compiled))
		(progn
		  (load file)
		  (byte-compile-file file)
		  (kill-buffer "*Compile-Log*"))
	  (load file-compiled)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting the user configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getFromList(list index)
  (if (not (equal nil list))
	  (if (equal index (car (car list)))
		  (cdr (car list))
		(getFromList (cdr list) index))))

(defun getUserInfo(index)
  (getFromList user-setup index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load user setup file
;;
(setq user_setup_file "user_setup_options.el")
(setq user_setup_file_full (concat custom-load-path (concat "/" user_setup_file)))
(if (not (file-exists-p user_setup_file_full))
	(load-compile "user_setup.el"))
(if (not (file-exists-p user_setup_file_full))
	(message "Error when reading user customization file!")
  (load-compile user_setup_file))

;; load our default configuration file
;;
(load-compile "default_configuration.el")

;; load optional user configuration
;;
(load-compile "user_configuration.el" t)

;; load our default configuration file
;;
(setq custom_setup_file "custom_options.el")
(if (file-exists-p custom_setup_file)
    (load-compile custom_setup_file))

;;; .emacs.el ends here