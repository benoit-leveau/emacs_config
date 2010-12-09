;;; xStream_batch.el --- define functions for easy xStream compilation

;; Copyright (C) 2008  Benoit Leveau

;; Author: Benoit Leveau <benoit.leveau@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(defun xstream-compile-lightwave (version)
  "Compile the xStream plugin for LightWave"
  (interactive "sVersion of LightWave3D (default 9.5): ")
  (save-some-buffers t)
  (if (equal version "9.3")
      (if config-pc
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_lightwave_93.bat")
	(set (make-local-variable 'compile-command)
	     "cd /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_LightWave/ && xcodebuild -project \"xStreamPlugin_Lightwave.xcodeproj\" -target \"xStream for LW 9.3\" -configuration \"Release\" && cp /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_LightWave/build/Release/Vue7xStream.plugin/Contents/MacOS/Vue7xStream \"/Applications/LightWave 3D 9.3 UB/SharedSupport/Plugins/utility/Vue7xStream.plugin/Contents/MacOS/Vue7xStream\""))
    (if config-pc
	(set (make-local-variable 'compile-command)
	     "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_lightwave_95.bat")
	(set (make-local-variable 'compile-command)
	     "cd /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_LightWave/ && xcodebuild -project \"xStreamPlugin_Lightwave.xcodeproj\" -target \"xStream for LW 9.5\" -configuration \"Release\" && cp /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_LightWave/build/Release/Vue7xStream.plugin/Contents/MacOS/Vue7xStream \"/Applications/LightWave 3D 9.5 UB/SharedSupport/Plugins/utility/Vue7xStream.plugin/Contents/MacOS/Vue7xStream\"")))
  (message "Compiling LightWave3D plugin")
  (recompile))

(defun xstream-compile-maya (version)
  "Compile the xStream plugin for Maya"
  (interactive "sVersion of Maya (default 2008): ")
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_maya_2008.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_Maya/ && xcodebuild -project \"xStreamPlugin_Maya.xcodeproj\" -target \"xStream for Maya 8.0\" -configuration \"Deployment\" && cp /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_Maya/build/Deployment/Vue7xStream_v8.bundle /Applications/Alias/maya8.0/Maya.app/Contents/MacOS/plug-ins/Vue7xStream.bundle"))
  (recompile))

(defun xstream-compile-mentalray-maya (version)
  "Compile the xStream plugin for Mentalray (Maya)"
  (interactive "sVersion of Maya (default 2008): ")
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_mentalray_maya_2008.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_MentalRay/ && xcodebuild -project \"xStreamPlugin_MentalRay.xcodeproj\" -target \"MentalRay for Maya 8.0\" -configuration \"Deployment\" && cp /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_MentalRay/build/Deployment/Vue7xStream_v8.so /Applications/Alias/maya8.0/Maya.app/Contents/mentalray/lib/Vue7xStream.so"))
  (recompile))

(if config-pc
    (defun xstream-compile-mentalray-max (version)
      "Compile the xStream plugin for Mentalray (Max)"
      (interactive "sVersion of 3ds Max (default 2008): ")
      (save-some-buffers t)
      (if (equal version 9)
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_mentalray_max_9.bat")
	(if (equal version 2009)
	    (set (make-local-variable 'compile-command)
		 "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_mentalray_max_2009.bat")
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_mentalray_max_2008.bat")))
      (message "Compiling 3ds Max plugin")
      (recompile))
  )

(if config-pc
    (defun xstream-compile-mentalray-xsi (version)
      "Compile the xStream plugin for Mentalray (XSI)"
      (interactive "sVersion of 3ds Max (default 7): ")
      (save-some-buffers t)
      (if (equal version 5)
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_mentalray_xsi_5.bat")
	(if (equal version 6)
	    (set (make-local-variable 'compile-command)
		 "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_mentalray_xsi_6.bat")
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_mentalray_xsi_7.bat")))
      (message "Compiling 3ds Max plugin")
      (recompile))
  )

(if config-pc
    (defun xstream-compile-max (version)
      "Compile the xStream plugin for Max"
      (interactive "sVersion of 3ds Max (default 2008): ")
      (save-some-buffers t)
      (if (equal version 9)
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_max_9.bat")
	(if (equal version 2009)
	    (set (make-local-variable 'compile-command)
		 "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_max_2009.bat")
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_max_2008.bat")))
      (message "Compiling 3ds Max plugin")
      (recompile))  
  )

(defun xstream-compile-cinema4d (version)
  "Compile the xStream plugin for Cinema4D"
  (interactive "sVersion of Cinema4D (default 11): ")
  (save-some-buffers t)
  (if (equal version 9)
      (if config-pc
	   (set (make-local-variable 'compile-command)
		"cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_cinema4d.bat")
	(message "No version 9 on Mac!"))
    (if (equal version 10)
	(if config-pc
	    (set (make-local-variable 'compile-command)
		 "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_cinema4d_10.bat")
	  (set (make-local-variable 'compile-command)
	       "cd /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_C4D/ && xcodebuild -project \"xStreamPlugin_C4D.xcodeproj\" -target \"xStream for Cinema4D R10\" -configuration \"Release\" && cp /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_C4D/build/Release/Vue7xStream_v10.bundle/Contents/MacOS/Vue7xStream_v10 \"/Applications/CINEMA 4D R10/plugins/Vue7xStream/Vue7xStream.dylib\""))
      (if config-pc
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_cinema4d_11.bat")
	(set (make-local-variable 'compile-command)
	     "cd /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_C4D/ && xcodebuild -project \"xStreamPlugin_C4D.xcodeproj\" -target \"xStream for Cinema4D R11\" -configuration \"Release\" && cp /Users/bleveau/Vue/Source/xStreamPlugins/xStreamPlugin_C4D/build/Release/Vue7xStream_v11.bundle/Contents/MacOS/Vue7xStream_v11 \"/Applications/CINEMA 4D R11/plugins/Vue7xStream/Vue7xStream.dylib\""))))
  (message "Compiling Cinema4D plugin")
  (recompile))

(if config-pc
    (defun xstream-compile-xsi (version)
      "Compile the xStream plugin for XSI"
      (interactive "sVersion of XSI (default 7): ")
      (save-some-buffers t)
      (if (equal version 5)
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_xsi_5.bat")
	(if (equal version 6)
	    (set (make-local-variable 'compile-command)
		 "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_xsi_6.bat")
	  (set (make-local-variable 'compile-command)
	       "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_xsi_7.bat")))
      (message "Compiling Cinema4D plugin")
      (recompile))
  )

(defun xstream-compile-core ()
  "Save all buffers, and recompile xstream core"
  (interactive)
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_core.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/release; /Users/jvalgent/preMake && make -j 6"))
  (message "Compiling core project...")
  (recompile))

(defun xstream-compile-core-debug ()
  "Save all buffers, and recompile xstream core"
  (interactive)
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_core_debug.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/debug; /Users/jvalgent/preMake && make -j 6"))
  (message "Compiling core project in debug...")
  (recompile))

(defun xstream-compile-64core ()
  "Save all buffers, and recompile xstream core 64bit"
  (interactive)
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_core_64.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/debug; /Users/jvalgent/preMake && make -j 6"))
  (message "Compiling core 64bit project...")
  (recompile))

(defun xstream-compile-infinite ()
  "Save all buffers, and recompile xstream standalone"
  (interactive)
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_infinite.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/release_Infinite; /Users/jvalgent/preMake && make -j 6"))
  (message "Compiling infinite project...")
  (recompile))

(defun xstream-compile-infinite-debug ()
  "Save all buffers, and recompile xstream standalone in debug"
  (interactive)
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_infinite_debug.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/debug_Infinite; /Users/jvalgent/preMake && make -j 6"))
  (message "Compiling infinite project...")
  (recompile))

(if config-pc
    (defun xstream-compile-engine ()
      "Save all buffers, and recompile Engine"
      (interactive)
      (save-some-buffers t)
      (set (make-local-variable 'compile-command)
	    "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_engine.bat")
       (message "Compiling Engine project...")
       (recompile)))

(if config-pc
    (defun xstream-compile-all ()
      "Compile all xStream plugins"
      (interactive)
      (save-some-buffers t)
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/compile_all_plugins.bat")
      (message "Compiling all xStream plugins...")
      (recompile))
  )

(defun xstream-dired-logs ()
  (interactive)
  (dired "C:/Users/bleveau/AppData/Roaming/e-on software/Vue 7 xStream/Logs")
)

(defun xstream-dired-clipboards ()
  (interactive)
  (dired "x:/clipboards")
)

(global-set-key (kbd "C-x x d") 'xstream-dired-logs)

(defun xstream-debug-vue ()
  (interactive)
  (set (make-local-variable 'default-directory)
       "K:/Projects/Vue7/Program/application/")
  (cdb "cdb_e.exe DebugVue7Inf.exe")
)

(defun xstream-debug-maya ()
  (interactive)
  (set (make-local-variable 'default-directory)
       "C:/Program Files (x86)/Autodesk/Maya2008/bin/")
  (cdb "cdb_e.exe maya.exe")
)

(defun xstream-debug-lightwave ()
  (interactive)
  (set (make-local-variable 'default-directory)
       "C:/Program Files (x86)/NewTek/LightWave 3D 9/Programs/") 
  (cdb "cdb_e.exe lightwav.exe")
)

(defun xstream-debug-cinema4d ()
  (interactive)
  (set (make-local-variable 'default-directory)
       "c:/Program Files (x86)/Maxon/CINEMA 4D R11/")
  (cdb "cdb_e.exe CINEMA 4D.exe")
)

(defun xstream-debug-xsi ()
  (interactive)
  (set (make-local-variable 'default-directory)
       "c:/Softimage/XSI_7.0/Application/bin/")
  (cdb "cdb_e.exe XSI.exe")
)

(defun xstream-debug-max ()
  (interactive)
  ;(set (make-local-variable 'default-directory)
  ;     "c:/Program Files (x86)/Autodesk/3ds Max 2008/")
  (cdb "cdb_e.exe c:/Program Files (x86)/Autodesk/3ds Max 2008/3dsmax.exe")
)

(defun xstream-svn-source ()
  (interactive)
  ;(set (make-local-variable 'default-directory)
  ;     "c:/Program Files (x86)/Maxon/CINEMA 4D R11/")
  (svn-status "k:/Projects/Vue7/Source")
)

(defun xstream-svn-plugins ()
  (interactive)
  ;(set (make-local-variable 'default-directory)
  ;     "c:/Program Files (x86)/Maxon/CINEMA 4D R11/")
  (svn-status "k:/Projects/Vue7/Source/xStreamPlugins/")
)

(defun xstream-project (project)
  "Smart Compilation of xStream"
  (interactive "sProject to compile (default core): ")
  (if (equal project "")
      (xstream-compile-core)
    (if (equal project "clip")
	(xstream-dired-clipboards)
      (if (equal project "core-debug")
	  (xstream-compile-core-debug)
	(if (or (equal project "tag") (equal project "tags"))
	    (xstream-generate-tags)
	  (if (equal project "all")
	      (xstream-compile-all)
	    (if (equal project "s-p")
		(xstream-svn-plugins)
	      (if (equal project "s-v")
		  (xstream-svn-source)
		(if (or (equal project "max") (equal project "3ds"))
		    (xstream-compile-max "2008")
		  (if (equal project "d-max")
		      (xstream-debug-max)
		    (if (equal project "max9")
			(xstream-compile-max "9")
		      (if (equal project "mrmax")
			  (xstream-compile-mentalray-max "2008")
			(if (equal project "maya")
			    (xstream-compile-maya "2008")
			  (if (equal project "d-maya")
			      (xstream-debug-maya)
			    (if (equal project "mrmaya")
				(xstream-compile-mentalray-maya "2008")
			      (if (equal project "d-xsi")
				  (xstream-debug-xsi)
				(if (equal project "xsi")
				    (xstream-compile-xsi "7")
				  (if (equal project "mrxsi")
				      (xstream-compile-mentalray-xsi "7")
				    (if (equal project "d-c4d")
					(xstream-debug-cinema4d)
				      (if (equal project "c4d")
					  (xstream-compile-cinema4d "11")
					(if (equal project "d-lw")
					    (xstream-debug-lightwave)	    
					  (if (equal project "lw")
					      (xstream-compile-lightwave "9.5")
					    (if (equal project "lw93")
						(xstream-compile-lightwave "9.3")
					      (if (equal project "lw95")
						  (xstream-compile-lightwave "9.5")
						(if (equal project "core")
						    (xstream-compile-core)
						  (if (equal project "64core")
						      (xstream-compile-64core)
						    (if (equal project "engine")
							(xstream-compile-engine)
						      (if (or (or (equal project "infinite") (equal project "vue")) (or (equal project "standalone") (equal project "inf")))
							  (xstream-compile-infinite)
							(if (equal project "vue-debug")
							    (xstream-compile-infinite-debug)
							  (if (equal project "d-vue")
							      (xstream-debug-vue)
							    (if (or (or (equal project "d-infinite") (equal project "d-inf")) (or (equal project "d-vue") (equal project "d-standalone")))
								(xstream-compile-infinite)
							      (message "Unknown project! (choose one of the following: all, tag, max, mrmax, maya, mrmaya, xsi, mrxsi, c4d, lw, core, 64core, engine, infinite, s-p, s-v, d-max, d-c4d, d-lw, d-xsi"))))))))))))))))))))))))))))))))
  
(defun xstream-compile-crashsender ()
  "Compile the Crash Sender for Infinite"
  (interactive)
  (save-some-buffers t)
  (set (make-local-variable 'compile-command)
       "cd /Users/bleveau/Vue/crashsender; /Users/jvalgent/preMake && make -j 6 && cp /Users/bleveau/Vue/crashsender/CrashSender.app/Contents/MacOS/CrashSender /Users/bleveau/Vue/Debug_Infinite/Vue/.CrashSender.app/Contents/MacOS/CrashSender")
  (recompile))

(defun xstream-generate-tags ()
  "Save all buffers, and regenerate TAGS"
  (interactive)
  (save-some-buffers t)
  (if config-pc
      (set (make-local-variable 'compile-command)
	   "cd k:/Projects/Vue7/Source && K:/Projects/Vue7/Batch/generate_tags.bat")
    (set (make-local-variable 'compile-command)
	 "cd /Users/bleveau/Vue/Source; find ./ -maxdepth 4 \( -iname '*.cpp' -o -iname '*.h' -o iname '*.rh' -o -iname '*.inl' \) | etags --ignore-indentation --members -"))
  (recompile))

(global-set-key (kbd "C-)") 'xstream-project)

(provide 'xStream_batch)
;;; xStream_batch.el ends here
