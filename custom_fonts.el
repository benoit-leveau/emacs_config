;;; custom_fonts.el --- Customization of fonts and colors

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

 (setq
  x-fixed-font-alist
  '("Font Menu"
	("Misc"
	 ("6x12" "-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-*-1")
	 ("6x13" "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-*-1")
	 ("lucida 9"
	  "-b&h-lucidatypewriter-medium-r-normal-sans-0-0-0-0-m-0-*-1")
	 ("7x13" "-misc-fixed-medium-r-normal--13-120-75-75-c-70-*-1")
	 ("7x14" "-misc-fixed-medium-r-normal--14-130-75-75-c-70-*-1")
	 ("9x15" "-misc-fixed-medium-r-normal--15-140-*-*-c-*-*-1")
	 ("")
	 ("clean 8x8" "-schumacher-clean-medium-r-normal--*-80-*-*-c-*-*-1")
	 ("clean 8x14" "-schumacher-clean-medium-r-normal--*-140-*-*-c-*-*-1")
	 ("clean 8x10" "-schumacher-clean-medium-r-normal--*-100-*-*-c-*-*-1")
	 ("clean 8x16" "-schumacher-clean-medium-r-normal--*-160-*-*-c-*-*-1")
	 ("")
	 ("sony 8x16" "-sony-fixed-medium-r-normal--16-120-100-100-c-80-*-1")
	 ("")
	 ("-- Courier --")
	 ("Courier 10" "-adobe-courier-medium-r-normal--*-100-*-*-m-*-*-1")
	 ("Courier 12" "-adobe-courier-medium-r-normal--*-120-*-*-m-*-*-1")
	 ("Courier 14" "-adobe-courier-medium-r-normal--*-140-*-*-m-*-*-1")
	 ("Courier 18" "-adobe-courier-medium-r-normal--*-180-*-*-m-*-*-1")
	 ("Courier 18-b" "-adobe-courier-bold-r-normal--*-180-*-*-m-*-*-1")
	 )))

(set-face-attribute 'default nil :height 90)
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Monospace-9"))

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; TeX & LaTeX Stuff...
(setq tex-dvi-view-command "xdvi")
(setq tex-show-queue-command "lpstat -O")
(setq tex-dvi-print-command "dvips")
(setq tex-alt-dvi-print-command
	  '(format "dvips * -o %s" (read-string "Name of postscript file:")))

;; (setq-default c-auto-newline t)

;;; Construit les faces pour le font-lock-mode
(cond (window-system
	   (make-face 'font-lock-comment-face)
	   (make-face 'font-lock-function-name-face)
	   (make-face 'font-lock-keyword-face)
	   (make-face 'font-lock-reference-face)
	   (make-face 'font-lock-string-face)
	   (make-face 'font-lock-type-face)
	   (make-face 'font-lock-variable-name-face)

	   (setq font-lock-maximum-decoration t)))

;			  Face-name					   FG-Color	   BG-Color	Stippe	  Bold Ital. Under
;-----------------------------------------------------------------------------------------
(modify-face 'font-lock-comment-face	   "green"		 nil	nil	  nil   t	 nil)
(modify-face 'font-lock-function-name-face "yellow"		 nil	nil	  t	    nil	 nil)
(modify-face 'font-lock-keyword-face	   "lightGreen"	 nil	nil	  t     nil	 nil)
(modify-face 'font-lock-reference-face	   "lightBlue"	 nil	nil	  t	    nil	 nil)
(modify-face 'font-lock-string-face		   "pink"		 nil	nil	  t	    nil	 nil)
(modify-face 'font-lock-type-face		   "sandy brown" nil	nil	  nil   nil	 nil)
(modify-face 'font-lock-variable-name-face "grey"		 nil	nil	  nil   t	 nil)

;; my lovely background
(set-background-color "DarkSlateBlue")
(set-foreground-color "white")

;; cursors
(set-cursor-color "yellow")
(set-mouse-color "white")
(setq x-nontext-pointer-shape "crosshair")
(setq x-pointer-background-color "Yellow")
(setq x-pointer-foreground-color "HotPink")

(provide 'custom_fonts)
;;; custom_fonts.el ends here
