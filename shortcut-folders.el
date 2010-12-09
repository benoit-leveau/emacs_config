;;; shortcut-folders.el --- 
;;
;; Copyright (C) 2009  Benoit Leveau
;; Author: Benoit Leveau <benoit.leveau@gmail.com>
;; Keywords: 
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; Provide shortcut to everyday folders
;;
;; Code:

(defun shortcut-folders (project)
  "Shortcuts for everyday folders"
  (interactive "sDired to folder  (default ~/dev): ")
  (let ((processed t))
	(if (equal project "")
		(dired "~/dev")
	  (if (equal project "des")
		  (dired "~/Desktop")
		(if (equal project "tools")
			(dired "/software/tools")
		  (if (equal project "soft")
			  (dired "/software")
			(if (equal project "mpcmake")
				(dired "/usr/people/benoit-l/dev/svn/build/mpcMake/trunk")
			  (if (equal project "core")
				  (dired "/usr/people/benoit-l/dev/svn/core")
				(if (equal project "conf")
					(dired "~/config/")
				  (if (or (equal project "muggins") (equal project "mg") (equal project "mgn"))
					  (dired "/usr/people/benoit-l/dev/svn/core/muggins/trunk")
					(if (or (equal project "gsc") (equal project "giggleScripts"))
						(dired "/software/tools/gubbins/giggle/6.4/scripts/gglUtils/135")
					  (if (or (equal project "mugginsGL") (equal project "mgl"))
						  (dired "/usr/people/benoit-l/dev/svn/core/mugginsGL/trunk")
						(if (or (equal project "mpcMake") (equal project "mpcmake") (equal project "make") (equal project "mk"))
							(dired "/usr/people/benoit-l/dev/svn/build/mpcMake/trunk")
						  (setq processed nil))))))))))))
	(if processed
		(revert-buffer)
	  (message (concat "Unknown folder: " project))))
  )

(defun shortcut-folders-init ()
  "Setups the list of folders."
  (interactive))

(provide 'shortcut-folders)
;; shortcut-folders.el ends here