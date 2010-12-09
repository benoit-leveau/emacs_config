;;; custom_fonts.el --- Customization of fonts and colors

;; Copyright (C) 2009  Benoit Leveau

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


;             Face-name                    FG-Color    BG-Color  Stippe   Bold Ital. Under
;-----------------------------------------------------------------------------------------
(modify-face 'font-lock-comment-face       "green"       nil     nil      nil  t    nil)
(modify-face 'font-lock-function-name-face "yellow"      nil     nil      t   nil   nil)
(modify-face 'font-lock-keyword-face       "white"       nil     nil      t   nil   nil)
(modify-face 'font-lock-reference-face     "lightBlue"   nil     nil      t    nil  nil)
(modify-face 'font-lock-string-face        "pink"        nil     nil      t   nil   nil)
(modify-face 'font-lock-type-face          "Yellow"      nil     nil      nil nil   nil)
(modify-face 'font-lock-variable-name-face "White"       nil     nil      nil  t    nil)

(set-background-color "DarkSlateBlue")

(provide 'custom_fonts)
;;; custom_fonts.el ends here
