;;; user_setup.el --- User setup

;; Copyright (C) 2010  Benoit Leveau

;; Author: Benoit Leveau <benoit-l@moving-picture.com>
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

(setq user-setup-string
	  (concat 
	   "(setq user-setup "
	   "'("
	   "(\"name\" . \"" (read-from-minibuffer "Your name: ") "\")"
	   "(\"email\" . \"" (read-from-minibuffer "Your email: ") "\")"
	   "(\"location-name\" . \"" (read-from-minibuffer "Your location: ") "\")"
	   "(\"location_based-mode\" . t )"
	   "(\"company-name\" . \"" (read-from-minibuffer "Your company: ") "\")"
	   "(\"default-font-height\" . 90 )"
	   "(\"use-tabs\" . \\" (read-from-minibuffer "Use tabs (t/nil): ") "\")"
	   "(\"max-column-width\" . \\" (read-from-minibuffer "Max column width: ") "\")"
	   "))"))

(write-region user-setup-string nil user_setup_file_full)

;;; user_setup.el ends here
