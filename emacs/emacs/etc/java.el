(defun init-java ()
  (message "Initializing java")
  (setq-default tab-width 4
		;; Use spaces instead of tabs
		indent-tabs-mode nil) 
  (add-hook 'java-mode 'delete-trailing-whitespace-hook))

(add-hook 'java-mode-hook 'init-java)
