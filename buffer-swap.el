;;; buffer-swap.el --- 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Based on buffer-move.el 
;; Copyright (C) 2004  Lucas Bonnet <lukhas@free.fr>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; This file is for lazy people wanting to swap buffers without
;; typing C-x b on each window. This is useful when you have :
;;
;; +--------------+-------------+
;; |              |             |
;; |    Pif       |    Paf      |
;; |              |             |
;; +--------------+-------------+
;; |                            |
;; |            Pouf            |
;; |                            |
;; +----------------------------+
;;
;; and you want to have :
;;
;; +--------------+-------------+
;; |              |             |
;; |    Paf       |   Pif       |
;; |              |             |
;; +--------------+-------------+
;; |                            |
;; |            Pouf            |
;; |                            |
;; +----------------------------+
;;
;;; Code:

(require 'windmove)

(defun buffer-swap-and-move-to-top (other-win)
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  ;; swap top with this one
  (set-window-buffer (selected-window) (window-buffer other-win))
  ;; move this one to top
  (set-window-buffer other-win buf-this-buf)
  (select-window other-win))

(defun buffer-swap ()
"Swap the current buffer and another buffer (on any side of the current one).
The function first tries to swap with a buffer at the right, then at the left, then at the top, and finally at the bottom. If there is no split, ie there is only one buffer, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
	(let* ((other-win (windmove-find-other-window 'left))
	       (buf-this-buf (window-buffer (selected-window))))
	  (if (null other-win)
	      (let* ((other-win (windmove-find-other-window 'up))
		     (buf-this-buf (window-buffer (selected-window))))
		(if (null other-win)
		    (let* ((other-win (windmove-find-other-window 'down))
			   (buf-this-buf (window-buffer (selected-window))))
		      (if (null other-win)
			  (error "No windows to swap!")
			(buffer-swap-and-move-to-top other-win)))
		  (buffer-swap-and-move-to-top other-win)))
	    (buffer-swap-and-move-to-top other-win)))
      (buffer-swap-and-move-to-top other-win))))

(provide 'buffer-swap)
;;; buffer-swap.el ends here
