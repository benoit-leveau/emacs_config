;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computer-specific switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Detecting computer configuration...")

;; Global switches
;;
(setq config-pc t)
(setq config-mac nil)
(setq config-linux nil)

(setq config-eon nil)
(setq config-mpc nil)

(if (or (equal system-name "prudish.mpc.local")
	(equal system-name "capri.mpc.local"))
    ;; Shide Computer at MPC
    (progn
      (message "Computer 'Shide': LINUX-MPC")
      (setq config-pc nil)
      (setq config-linux t)
      (setq config-mpc t))
  (if (equal system-name "TOTORO")
      ;; Vista64 at e-on
      (progn
	(message "Computer 'Totoro': PC-EON")
	(setq config-eon t))
    (if (equal system-name "QUAD")
	;; Shared MacOS at e-on
	(progn
	  (message "Computer 'Quad': MAC-EON")
	  (setq config-pc nil)
	  (setq config-mac nil)
	  (setq config-eon t))
      ;; home computer
      (progn
	(message "Computer 'Unknown': PC-NOT_EON")))))

;; Main Drive Letter is E at home...
(if config-eon
    (setq main_drive_E nil)
  (setq main_drive_E t))

;; directory to put various .el files into
;;
(if config-pc
    (if main_drive_E
	(setq custom-load-path "E:/emacs_includes")
      (setq custom-load-path "C:/emacs_includes"))
  (if config-linux
      (if config-mpc
	  (setq custom-load-path "~/emacs_includes")
	(setq custom-load-path "~/lib/emacs/emacs_includes"))
    (setq custom-load-path "~/lib/emacs/emacs_includes")))

(setq load-path (cons custom-load-path load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading (and compiling if necessary) custom .emacs file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-compile (file_to_load)
  "Load a compile version of the file (or compile it if necessary)"
  (interactive "sFile to load: ")
  (setq file (concat custom-load-path (concat "/" file_to_load)))
  (setq file-compiled (concat file "c"))
  
  ;; Make sure source file exists
  (if (not (file-exists-p file))
    (message (concat "Can't load " (concat file_to_load ". Make sure path is correct!"))))

  ;; Load compiled version of file (compile the .el file if necessary)
  (if (or (not (file-exists-p file-compiled)) (file-newer-than-file-p file file-compiled))
      (progn
	(load file)
	(byte-compile-file file)
	(kill-buffer "*Compile-Log*"))
    (load file-compiled))
)

;; load or custom .emacs file
(message "Loading custom .emacs file...")
(load-compile "custom_emacs.el")
