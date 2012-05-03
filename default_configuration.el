;;; default_configuration.el --- Emacs Default Configuration File

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name (getUserInfo "name"))

;; Save every outgoing message to a file
(setq mail-archive-file-name "~/mail/sent-items")
  
;; Home settings
(setq smtpmail-default-smtp-server "mail")
(setq mail-host-address "mail")
(setq smtpmail-local-domain "mail")
(setq user-mail-address (getUserInfo "email"))

;; My Address Book
;; You can put your addresses here. They will be expanded as you type in in
;; the mail mode
; (define-mail-abbrev "foo" "foo[at]foo.com")

;; calendar
(setq calendar-location-name (getUserInfo "location-name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Dired-x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WcyDesktop mode
;; http://www.emacswiki.org/emacs/WcyDesktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Desktop files
;;
(load-compile "wcy-desktop.el")
(require 'wcy-desktop)
(define-key wcy-desktop-key-map (kbd "C-x") nil)
(define-key wcy-desktop-key-map (kbd "C-x k") 'kill-buffer)
(define-key wcy-desktop-key-map (kbd "C-\\") nil)
(define-key wcy-desktop-key-map (kbd "C-'") nil)
(define-key wcy-desktop-key-map (kbd "C-/") nil)
(wcy-desktop-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS2
;; http://code.google.com/p/js2-mode/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-compile "js2.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UndoTree mode
;; http://www.emacswiki.org/emacs/UndoTree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-compile "undo-tree.el")
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Column Marker Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-compile "column-marker.el")
(column-marker-1 (getUserInfo "max-column-width"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading edition modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; loads ruby mode when a .rb file is opened.
;;
(load-compile "ruby-mode.el")
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

;; CMakeLists mode
;;
(message "Loading CMake mode...")
(load-compile "cmake-mode.el")
(setq auto-mode-alist (append '(("CMakeLists\\.txt\\'" . cmake-mode)
				("\\.cmake\\'" . cmake-mode))
				  auto-mode-alist))

;; LUA mode
;;
(load-compile "lua-mode.el")
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ggl$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; PHP mode
;;
(message "Loading PHP mode...")
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(load-compile "php-mode.el")

;; MEL mode
;;
(message "Loading MEL mode...")
(setq auto-mode-alist (cons '("\\.mel$" . php-mode) auto-mode-alist))
(load-compile "mel-mode.el")

;; StumpWM mode
;;
(message "Loading StumpWM mode...")
(load-compile "stumpwm-mode.el")

;; Python mode
;;
(message "Loading Python mode...")
(if (not config-windows)
	(setq load-path (cons "/sw/lib/python2.2/Misc/" load-path)))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
			interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq auto-mode-alist (cons '("\\/SConstruct$" . python-mode) auto-mode-alist))

;; Makefile mode
;;
(setq auto-mode-alist (cons '("\\/Makefile$" . makefile-mode) auto-mode-alist)) ; Makefile
(setq auto-mode-alist (cons '("\\/Makefile\\.[A-Za-z0-9]+$" . makefile-mode) auto-mode-alist)) ; Makefile.foo
(setq auto-mode-alist (cons '("\\Make[A-Z][A-Za-z]*\\'"	. makefile-mode) auto-mode-alist)) ; MakeFoo

;; C++ mode
;;
(setq auto-mode-alist (append '(("\\.h\\'"	. c++-mode)
								("\\.inl\\'" . c++-mode)
								("\\.src\\'" . c++-mode))
							  auto-mode-alist))


;; loads ruby mode when a .rb file is opened.
;;
(setq load-path (cons (concat custom-load-path "/haskell-mode-2.1") load-path))
(load "haskell-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LocationBased mode
;; to distinguish between centrally installed files and local files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (getUserInfo "location_based-mode")
	(progn
	  (message "Loading location_based mode...")
	  (load-compile "location_based-mode.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CDB mode: debugging on Windows
;;
(if config-windows
	(progn
	  (message "Loading CDB mode...")
	  (load-compile "cdb-gud.el")))

;; SVN mode
;;
(message "Loading SVN mode...")
(load-compile "psvn.el")
(setq svn-status-verbose nil)

;; Git mode
;;
(message "Loading Git mode...")
(load-compile "git.el")
(load-compile "git-blame.el")

;; Provide a common vc-status function
;;
(defun vc-status (&optional check-remote)
  "Display the appropriate VC status window.
  If CVS, runs `cvs-examine'.
  If Git, runs `git-status'.
  If Subversion, runs `svn-status-update'.
  Prefix arg means check status against the remote repository (if SVN)."
  (interactive "P")
  (cond
   ((file-directory-p (expand-file-name "CVS"))
    (cvs-examine default-directory '("-d" "-P"))) ; Always checks remote repository
   ((file-directory-p (expand-file-name ".svn"))
    (if (get-buffer "*svn-process*")
        (svn-status-update check-remote)
      (svn-status default-directory check-remote)))
   ;; If git-get-top-dir doesn't error, then it's a GIT repository.
   ((condition-case nil
        (progn
          (and (fboundp 'git-status)
               (not (fboundp 'git-get-top-dir))
               (require 'git))
          (git-status (git-get-top-dir default-directory))
          t)
      (error nil)))
   (t (error "%s does not appear to be under version control" default-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BreadCrumb (global bookmarks)
;; http://breadcrumbemacs.sourceforge.net
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-compile "breadcrumb.el")
;(require 'breadcrumb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Buffer swapping
;;
(load-compile "buffer-swap.el")

;; Shortcut folders
;;
(load-compile "shortcut-folders.el")

;; loads thumb mode
;;
(autoload 'thumbs-show-all-from-dir "thumbs" "Preview images in a directory." t)

;; TODO comment
;;
(load-compile "dired-sort-map.el")

;; TODO comment & see if keep
;;
;; (load-compile "extraedit.el")

;; TODO comment & see if keep
;;
;; (load-compile "snippet.el")

;; Buffer swapping
;;
(load-compile "buffer-swap.el")

;; TODO comment
;;
(when (eq system-type 'windows-nt)
  (load-compile "w32-browser.el")
  (define-key dired-mode-map [(control return)] 'dired-w32-browser)
  (define-key dired-mode-map [(meta return)] 'dired-w32explore))
(when (not (eq system-type 'windows-nt))
  (define-key dired-mode-map [(control return)] 'dired-display-file)
  (define-key dired-mode-map [(meta return)] 'dired-display-file))

;; TODO comment
;;
(load-compile "find-recursive.el")

;; Insert random quotes in the status bar 
;; after some idle time
;;
(require 'random-idle-quote)
(random-idle-quote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setup default options...")

;; This is to not display the initial message (which says to a novice
;; user what to do first if he/she is confused).
;;
(setq inhibit-startup-message t)

;; so that several files in different folders will be distinguishable
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; abbreviation files
(read-abbrev-file (concat custom-load-path "/.abbrev_defs") t)
(setq save-abbrevs t)

;; Auto reload file
;;
(global-auto-revert-mode)

;; Sets emacs to prompt on close so you cannot close by accident!!
;;
; (setq confirm-kill-emacs 'yes-or-no-p)

;; Open unidentified files in text mode
;;
(setq default-major-mode 'text-mode)

;; Allows syntax highlighting to work, among other things
;;
(global-font-lock-mode t)

;; Deleted text won't go to kill-ring
;; also: Replace selection by what you type!
;;
(delete-selection-mode t)

;; Automagically read compressed files
;;
(auto-compression-mode 1)

;; Enable visual feedback on selections
;;
(setq transient-mark-mode t)

;; Display clock
;;
(display-time)
(setq display-time-24hr-format t)

;; disable any noise
;;
(setq bell-volume 0)
(setq visible-bell t)

;; hide the menu bar
;;
(menu-bar-mode -1)

;; hide the tool bar
;;
(tool-bar-mode -1)
(setq default-toolbar-visible-p nil)

;; hide the scroll bar
;;
(scroll-bar-mode nil)

;;iswitchb: convenienvient minor mode to switch between buffers
;;
(iswitchb-mode t)

;; Spell program
;;
; (setq ispell-program-name "/sw/bin/aspell")
(if (not config-windows)
	(setq-default ispell-program-name "aspell"))

;; Display line/column number in status bar
;;
(column-number-mode 1)
(line-number-mode 1)

;; Smooth buffer scrolling
;;
(setq scroll-step 1)

;; no blinking cursor
;;
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode nil))

;; Highlight current line
;;
(global-hl-line-mode 1)
(set-face-background 'hl-line "#403080")  ;; Emacs 22 Only

;; Highlight Parentheses
;;
(load-compile "highlight-parentheses.el")
(defun turn-on-highlight-parentheses-mode ()
(highlight-parentheses-mode t))
(define-global-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  turn-on-highlight-parentheses-mode)
(global-highlight-parentheses-mode)
(setq hl-paren-background-colors '("DarkSlateBlue"))

;; Turn off backup files.
;;
(setq make-backup-files nil)

;; TODO comment
;;
(setq backup-by-copying t)

;; TODO comment
;;
(setq version-control t)

;; TODO comment
;;
(setq delete-old-versions t)

;; Tabs & Indentation
;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode (getUserInfo "use-tabs"))

;; yes -> y, no -> n
;;
;(fset 'yes-or-no-p 'y-or-n-p)

;; TODO comment
;;
(setq process-connection-type t)

;; TODO comment
;;
(setq completion-ignore-case t)

;; For ff-find-other-file
;;
(setq cc-other-file-alist
	  '(("\\.cpp$" (".h" ".hpp" ".inl"))
		("\\.h$" (".inl" ".cpp" ".c"))
		("\\.hpp$" (".inl" ".cpp" ".c"))
		("\\.inl$" (".cpp" ".c" ".h" ".hpp"))
		("\\.rh$" (".erc"))
		("\\.erc$" (".rh"))
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'create-directory make-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grep Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ignore .build folders when grepping
;;
(eval-after-load "grep"
  '(progn
    (add-to-list 'grep-find-ignored-directories ".build")))

;; Setting the grep command 
;;
(if config-windows
	(setq grep-find-command '("cygfind \"k:/Projects/\" ( -iname '*.cpp' -o -iname '*.h' -o -iname '*.inl' -o -iname '*.rh' ) -exec grep -Hn '' {} ;" . 122))
  (setq grep-find-command '("find ~/Projects/ \\( -iname '*.cpp' -o -iname '*.h' -o -iname '*.inl' -o -iname '*.rh' \\) -exec grep -Hn '' {} \\;" . 120)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-frame-alist
	  (append
	   '((width . 120))
	   '((height . 90)) ; ((getUserInfo "default-frame-height")))) <= doesn't work... TODO
	   '((cursor-blink . nil))
	   default-frame-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors & Fonts & Faces & ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setup fonts...")
(load-compile "custom_fonts.el")

;; (setq load-path (cons (concat custom-load-path "/emacs-color-theme-solarized") load-path))
;; (enable-theme 'solarized-light)
;; (enable-theme 'solarized-dark)

(set-face-attribute 'default nil :height (getUserInfo "default-font-height"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-c-mode-common-hook ()
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode

 (c-set-offset 'substatement-open 0)

 (setq c++-tab-always-indent (getUserInfo "use-tabs"))
 (setq c-basic-offset 4)                  ;; Default is 2
 (setq c-indent-level 4)                  ;; Default is 2

 (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 4)
 (setq indent-tabs-mode (getUserInfo "use-tabs"))  ; use spaces only if nil
 (column-marker-1 (getUserInfo "max-column-width"))
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-python-mode-common-hook ()
  ;; my customizations for python-mode
  (column-marker-1 (getUserInfo "max-column-width"))
)

(add-hook 'python-mode-common-hook 'my-python-mode-common-hook)
  
(defun my-dired-mode-common-hook ()
  (global-set-key (kbd "\\") 'dired-up-directory))
(add-hook 'dired-mode-common-hook 'my-dired-mode-common-hook)

(setq buffers-menu-max-size nil)
(setq truncate-partial-width-windows nil)

(setq next-line-add-newlines nil)
;;(setq auto-save-default nil)

;; For autocompletion (like tab in tcsh) -> shift-tab
(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev
   try-expand-dabbrev-all-buffers))

(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region	   'disabled nil)

;; Deux espaces à la fin des phrases en anglais.
;;(setq sentence-end "[.?!][]\"')]*\\($\\| $\\|\t\\|  \\)[ \t\n]*")
;;(setq sentence-end-double-space t)

;; Une espace à la fin des phrases en français.
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space nil)

(add-hook 'after-save-hook 'mark-file-as-modified)

(add-hook 'find-file-hooks 'auto-insert)

(define-skeleton my-skeleton-c-cout
  "Insert a c cout statement" nil
  "std::cout <<\"" > _	"\" <<std::endl;")
(define-abbrev c++-mode-abbrev-table "$cout" "" 'my-skeleton-c-cout)

(load-library "autoinsert")

(setq auto-insert-alist
	  (append
	   '((("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
		  nil (concat "/*\n\n***********************************************************\n* © " (getUserInfo "company-name") ", inc. All rights reserved. *\n***********************************************************\n\n*/\n\n\n")
		  "#include \""
		  
		  ;; without checking for the file existence
		  (file-name-nondirectory
		   (file-name-sans-extension buffer-file-name))
		  ".h\"\n\n"))
	   
	   ;; with checking for the file existence
	   ;;		  (let
	   ;;			  ((stem
	   ;;			(file-name-sans-extension buffer-file-name)))
	   ;;			(cond
	   ;;			 ((file-exists-p
	   ;;			   (concat stem ".h"))
	   ;;			  (file-name-nondirectory
	   ;;			  (concat stem ".h")))
	   ;;			 ((file-exists-p
	   ;;			 (concat stem ".hh"))
	   ;;			  (file-name-nondirectory
	   ;;			  (concat stem ".hh")))))
	   ;;		  & "\"\n\n" | -10))
	   auto-insert-alist))

(setq auto-insert-alist
	  (append
	   '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
		  (upcase
		   (concat
			(file-name-nondirectory
			 (substring buffer-file-name 0
						(match-beginning 0)))
			"_"
			(substring buffer-file-name
					   (1+
						(match-beginning 0)))))
		  (concat "/*\n\n***********************************************************\n* © " (getUserInfo "company-name") ", inc. All rights reserved. *\n***********************************************************\n\n*/\n\n")
		  "#ifndef " str n "#define " str
		  "\n\n" _ "\n\n#endif // !" str "\n"))
	   auto-insert-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WinRing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-compile "winring.el")
(winring-initialize)

(defun winring-jump-or-create (&optional name)
  "Jump to or create configuration by name"
  (interactive)
  (let* ((ring (winring-get-ring))
	 (n (1- (ring-length ring)))
	 (current (winring-name-of-current))
	 (lst (list (cons current -1)))
	 index item)
	(while (<= 0 n)
	  (push (cons (winring-name-of (ring-ref ring n)) n) lst)
	  (setq n (1- n)))
	(setq name
	  (or name
		  (completing-read
		   (format "Window configuration name (%s): " current)
		   lst nil 'confirm nil 'winring-name-history current)))
	(setq index (cdr (assoc name lst)))
	(if (eq nil index)
	(progn
	  (winring-save-current-configuration)
	  (delete-other-windows)
	  (switch-to-buffer winring-new-config-buffer-name)
	  (winring-set-name name))
	  (when (<= 0 index)
	(setq item (ring-remove ring index))
	(winring-save-current-configuration)
	(winring-restore-configuration item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DirTrack in term mode
;; aka. how emacs should know what's the pwd in a shell?
;; default is tracking your cd commands, but doesn't work with aliases, job
;; commands, etc.,
;; dirtrack is looking at the prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq dirtrack-list '("^benoit-l@[a-zA-z]+:" 1 ))
;; (require 'dirtrack)
;; (add-hook 'shell-mode-hook
;;			 #'(lambda ()
;;				 (dirtrack-mode 1)
;;				 (add-hook 'comint-preoutput-filter-functions
;;						   'dirtrack-filter-out-pwd-prompt t t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'edmacro)
(setq last-kbd-macro (edmacro-parse-keys "C-a TAB C-SPC C-e M-w M-m std 2*: cout C-q SPC 2*< SPC \" C-e : SPC \" SPC 2*< C-y SPC 2*< SPC std 2*: endl ;"))
(name-last-kbd-macro 'show)

(defun save-all-buffers ()
  "Save all buffers"
  (interactive)
  (save-some-buffers t))

(defun 1win ()
  "1 window"
  (interactive)
  (set-frame-width (selected-frame) 120)
  (delete-other-windows))

(defun 2win ()
  "2 windows"
  (interactive)
  (set-frame-width (selected-frame) 244)
  (delete-other-windows)
  (split-window-horizontally))

(defun 3win ()
  "3 windows"
  (interactive)
  (set-frame-width (selected-frame) 361)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun my_init ()
  "Init"
  (interactive)
  (2win)
  (next-buffer)
  (next-buffer)
  (wcy-desktop-load-file)
)

(defun open-same-buffer-other-window ()
  "Open in other window"
  (interactive)
  (setq current buffer-file-name)
  (other-window 1)
  (find-file current)
  (other-window 1)
)

(require 'mm-url)
(defun google-define-word-or-phrase (query)
  (interactive "sInsert word or phrase to search: ")
  (let* ((url (concat "http://www.google.com/webhp?hl=en&q=define%3A"
			  (replace-regexp-in-string " " "+" query)))
	 (definition
	   (save-excursion
		 (with-temp-buffer
		   (mm-url-insert url)
		   (goto-char (point-min))
		   (if (search-forward "No definitions found of " nil t)
		   "No definitions found"
		 (buffer-substring (search-forward "<li>") (- (search-forward "<") 1)))))))
	(message "%s: %s" query definition)))

(defun th-display-buffer (buffer force-other-window)
  "If BUFFER is visible, select it.

If it's not visible and there's only one window, split the
current window and select BUFFER in the new window. If the
current window (before the split) is more than 165 columns wide,
split horizontally, else split vertically.

If the current buffer contains more than one window, select
BUFFER in the least recently used window.

This function returns the window which holds BUFFER.

FORCE-OTHER-WINDOW is ignored."
  (or (get-buffer-window buffer)
	  (if (one-window-p)
		  (let ((new-win (if (> (window-width) 165)
							 (split-window-horizontally)
						   (split-window-vertically))))
			(set-window-buffer new-win buffer)
			new-win)
		(let ((new-win (get-lru-window)))
		  (set-window-buffer new-win buffer)
		  new-win))))

(setq display-buffer-function 'th-display-buffer)

(defun copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
		(name (if (eq major-mode 'dired-mode)
				  (dired-get-filename)
				(or (buffer-file-name) ""))))
	(cond ((eq choice ?F)
		   (setq new-kill-string name))
		  ((eq choice ?D)
		   (setq new-kill-string (file-name-directory name)))
		  ((eq choice ?N)
		   (setq new-kill-string (file-name-nondirectory name)))
		  (t (message "Quit")))
	(when new-kill-string
	  (message "%s copied" new-kill-string)
	  (kill-new new-kill-string))))

;; unix2dos
(defun unix2dos()
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; dos2unix
(defun dos2unix()
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; Insert date into buffer
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y %k:%M:%S %z")))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
	(save-excursion
	  (next-line)
	  (transpose-lines 1))
	(next-line)
	(move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
	(save-excursion
	  (next-line)
	  (transpose-lines -1))
	(move-to-column col)))

(defun duplicate-line()
  (interactive)
  (let ((col (current-column)))
	(kill-whole-line 0)
	(yank)
	(open-line 1)
	(forward-char 1)
	(yank)
	(move-to-column col)))
(global-set-key (kbd "C-*") 'duplicate-line)

;; Compute the length of the marked region
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

(defun prepare-file ()
  (interactive)
  (delete-trailing-whitespace)
  (if (getUserInfo "use-tabs")
	  (tabify (point-min) (point-max))
	(untabify (point-min) (point-max)))
  (save-buffer))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
	(let ((end (copy-marker end)))
	  (while
		  (progn
			(goto-char start)
			(re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
		(replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun find-tag-at-point ()
  "*Find tag whose name contains TAGNAME.
  Identical to `find-tag' but does not prompt for
  tag when called interactively;  instead, uses
  tag around or before point."
	(interactive)
	  (find-tag (if current-prefix-arg
					(find-tag-tag "Find tag: "))
				(find-tag (find-tag-default))))

(defun upcase-previous-word ()
  (interactive)
  (upcase-word -1))

(defun prev-win ()
  "previous window"
  (interactive)
  (other-window -1))

;; pour aller a la parenthese correspondante (ou crochet, accolade, commentaires c)
(defun my-match-paren ()
	(interactive)
	(cond ((equal (char-before) 41) (backward-list 1))
		((equal (char-before) 93) (backward-list 1))
		((equal (char-before) 125) (backward-list 1))
		((equal (char-before) 62) (search-backward "<"))
		((and (equal (char-before) 47)
			 (equal (char-before (- (point) 1)) 42))
			(search-backward "/*"))
		((equal (char-after) 40) (forward-list 1))
		((equal (char-after) 91) (forward-list 1))
		((equal (char-after) 123) (forward-list 1))
		((equal (char-after) 60) (search-forward ">"))
		((and (equal (char-after) 47)
			 (equal (char-after (+ (point) 1)) 42)
			 (search-forward "*/")))))

;; ;; this version doesn't work well with shift
;;(defun back-to-indentation-or-beginning ()
;;	(interactive)
;;	(if (= (point) (save-excursion (back-to-indentation) (point)))
;;		(beginning-of-line)
;;	  (back-to-indentation)))

;; this version seems to work better
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if this-command-keys-shift-translated
	  (unless mark-active (push-mark nil t t))
	(when (and mark-active cua--last-region-shifted)
	  (deactivate-mark)))
  (if (= (point) (progn (back-to-indentation) (point)))
	  (beginning-of-line)))

(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
	Deletes whitespace at join."
	  (interactive "P")
	  (if (and (eolp) (not (bolp)))
		  (delete-indentation t)
		(kill-line arg)))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
	 (message "Copied line")
	 (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
	 (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defadvice yank (before slick-copy activate)
  "Position point when yanking lines."
  (let ((kill (current-kill 0 t)))
	(when (eq ?\n (elt kill (1- (length kill))))
	  (beginning-of-line))))

;;(setq scroll-step 0)
;;(setq scroll-margin 2)
;;(if (not config-windows)
;;	  (progn
;;		(setq explicit-shell-file-name "/bin/zsh")
;;		(setq calc-gnuplot-name "/sw/bin/gnuplot")))

(defun save-and-recompile ()
  "Save all buffers, and recompile"
  (interactive)
  (save-some-buffers t)
  (recompile))

(defun make-arg-list (strlist)
  (if (null strlist)
	  ""
	(concat "\"" (car strlist) "\" " (make-arg-list (cdr strlist)))))

(setq modified-files-list '())

(defun mark-file-as-modified ()
  "Mark the file as modified for compilation"
  (setq modified-files-list (add-to-list 'modified-files-list (buffer-file-name))))

;; fonction qui indente la ligne sur laquelle on Ã©tait et la suivante
;; quand on appuie sur entrÃ©e (en mode c c++)
(defun newline2()
  (interactive)
  (indent-according-to-mode)
  (open-line 1)
  (forward-char 1)
  (indent-according-to-mode))

; inserer des accolades
(defun my-insert-braces ()
  "Insert braces"
  (interactive "*")
  (newline2)
  (insert "{")
  (newline2)
  (newline2)
  (insert "}")
  (indent-according-to-mode)
  (previous-line)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Setup keyboard shortcuts...")

;; scrolling cursor and window 2 lines up/down
(global-set-key (kbd "S-<down>") '(lambda () (interactive) (progn (next-line 2) (scroll-up 2))))
(global-set-key (kbd "S-<up>") '(lambda () (interactive) (progn (next-line -2) (scroll-down 2))))

;; scrolling window 4 lines up/down
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (scroll-down 4)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (scroll-up 4)))

(global-set-key (kbd "C-.") 'find-tag-at-point)

(global-set-key [\C-M-down] 'move-line-down)
(global-set-key [\C-M-up] 'move-line-up)

(global-set-key (kbd "<M-u>") 'upcase-previous-word)
(global-set-key (kbd "<C-S-u>") 'upcase-word)

(global-set-key (kbd "C-:") 'help)

;; alt-g-g for goto-line (already working)
;;
; (global-set-key "\347\347" (quote goto-line))

;; Set M-p/n for previous/next error
;; (works for grep results, etc.)
;;
(global-set-key "\M-p" 'previous-error)
(global-set-key "\M-n" 'next-error)

;; Make control+pageup/down scroll the other buffer
;;
(global-set-key [C-next]  'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;; Ctrl-H is backspace, not help
;;
(global-set-key "\C-h" 'delete-backward-char)

(global-set-key [f1] 'info)
(global-set-key [f2] 'save-all-buffers)
(global-set-key [f3] 'save-buffer)
(global-set-key [f5] 'font-lock-mode)
(global-set-key [f6] '1win)
(global-set-key [f7] '2win)
(global-set-key [f8] 'kill-this-buffer)
(global-set-key [f9] 'font-lock-mode)
(global-set-key [f10] 'add-change-log-entry)

(global-set-key [delete] 'delete-char)
(global-set-key [C-delete] 'kill-word)
(global-set-key [C-^] 'goto-line)

(global-set-key [C-M-space] (quote just-one-space))
(global-set-key [4194431]	(quote delete-horizontal-space)) ; C-DEL

(global-set-key (kbd "C-<tab>") 'hippie-expand)
;; (global-set-key (kbd "C-/") 'hippie-expand))

(global-set-key [C-end] (quote end-of-buffer))
(global-set-key (quote [C-home]) (quote beginning-of-buffer))
(global-set-key (kbd "C--") 'undo)

(global-set-key "\C-xp" 'prev-win)
(global-set-key "\C-xo" 'other-window)

;; keyboard shortcut to access common projects
;;
(global-set-key (kbd "C-/") 'shortcut-folders)
;; (global-set-key (kbd "C-$") 'shortcut-folders))

; (global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key "\C-z" 'my-match-paren)

(global-set-key "\C-xri" 'string-insert-rectangle)

;; bind delete trailing
(global-set-key "\M-k" 'delete-trailing-whitespace)

;; Pour pouvoir inserer des retours a la ligne dans le minibuffer
(global-set-key [(control return)] 'newline)

(global-set-key [C-kp-add] '(lambda () (interactive) (enlarge-window-horizontally 1)))
(global-set-key [C-kp-subtract] '(lambda () (interactive) (enlarge-window-horizontally -1)))

(global-set-key [C-kp-multiply] '(lambda () (interactive) (enlarge-window 1)))
(global-set-key [C-kp-divide] '(lambda () (interactive) (enlarge-window -1)))

(global-set-key [M-kp-multiply] '(lambda () (interactive) (split-window-vertically)))
(global-set-key [M-kp-divide] '(lambda () (interactive) (split-window-horizontally)))

(global-set-key [end] 'end-of-line)
;;(global-set-key [home] 'beginning-of-line)
(global-set-key [home] 'back-to-indentation-or-beginning)

(global-set-key (kbd "C-\\") 'buffer-swap)
;; (global-set-key (kbd "C-!") 'buffer-swap))

;; Define the return key to avoid problems on MacOS X
;; (if (not config-windows)
;;	(define-key function-key-map [return] [13]))

(global-set-key "\C-k" 'kill-and-join-forward)

(global-set-key "\C-x\C-b" 'bs-show)

;; Breadcrumb shortcuts
;;
(global-set-key (kbd "C-x x s") 'bc-set)
(global-set-key (kbd "C-x x p") 'bc-previous)
(global-set-key (kbd "C-x x n") 'bc-next)
;(global-set-key (kbd "C-x x c") 'bc-local-previous)
;(global-set-key (kbd "C-x x c") 'bc-local-next)
;(global-set-key (kbd "C-x x c") 'bc-goto-current)
(global-set-key (kbd "C-x x l") 'bc-list)
;(global-set-key (kbd "C-x x c") 'bc-clear)
(global-set-key (kbd "C-<f9>") 'bc-previous)
(global-set-key (kbd "C-<f10>") 'bc-next)
(global-set-key (kbd "C-<f11>") 'bc-local-previous)
(global-set-key (kbd "C-<f12>") 'bc-local-next)

;; Active the mouse wheel:
;; Add scrolling with mouse
;;
(mouse-wheel-mode)
(global-set-key	  [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key	  [mouse-5] '(lambda () (interactive) (scroll-up   5)))
(global-set-key [S-mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [S-mouse-5] '(lambda () (interactive) (scroll-up   1)))
(global-set-key [C-mouse-5] '(lambda () (interactive) (scroll-up   (/ (window-height) 2))))
(global-set-key [C-mouse-4] '(lambda () (interactive) (scroll-down (/ (window-height) 2))))

(global-set-key [f2] 'my-insert-braces)

(global-set-key (kbd "C-=") 'ff-find-other-file)

(global-set-key (kbd "C-'") 'open-same-buffer-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-at-time "1 sec" nil (lambda () (my_init)))

;;; default_configuration.el ends here