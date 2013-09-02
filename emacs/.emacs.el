(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/deps")
(global-linum-mode)

(autoload 'magit-status "magit" nil t)

(defun delete-trailing-whitespace-hook ()
    (setq show-trailing-whitespace t)
    (make-local-hook 'before-save-hook)
    (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Key bindings
(global-set-key (kbd "C-x RET") 'compile)
(global-set-key "\M-\C-g" 'goto-line)

(when (file-exists-p "~/emacs/etc/lang.el")
  (load "~/emacs/etc/lang.el"))

(when (file-exists-p "~/emacs/etc/priv.el")
  (load "~/emacs/etc/priv.el"))

(when (file-exists-p "~/emacs/etc/elixir.el")
  (load "~/emacs/etc/elixir.el"))

;; Access to remote files
(defun init-tramp ()
  "Set tramp configuration."
  (setq tramp-default-user "khia")
  (setq tramp-default-method "ssh")
  (require 'tramp))
(when (locate-library "tramp")
  (init-tramp))

(set 'man-path "/usr/share/man")

;; ============================
;; Printer
;; ============================
;;(setq printer-name "Phaser_6350DT")
;;printer-name
;;(setq printer-name "PDF")
(setq printer-name "Brother_MFC")

;; ============================
;; Look & Feel
;; ============================
;; disable startup message
(setq inhibit-startup-message t)
;; Misc
;;    (global-set-key [C-tab] "\C-q\t")   ; Control tab quotes a tab.

;; Tag Completion
(global-set-key [C-tab] 'complete-tag)   ; Control tab quotes a tab.

(setq tags-table-list
           '("~/emacs" "~/dev"))
(setq visible-bell t)
;; Highlight the current line
(global-hl-line-mode 1)
;; Show paren
(show-paren-mode 1)
;; Status line
(setq-default column-number-mode t)
(setq-default line-number-mode t)
;; Window size
(enlarge-window 25)
;;(enlarge-window-horizontally 80)

;; filename in the window title
;;(setq frame-title-format '(buffer-file-name "%b - Emacs" "%b - Emacs"))
(setq frame-title-format '
      (buffer-file-name "%f - Emacs"
			(dired-directory dired-directory "%b")))
;;(setq icon-title-format '(buffer-file-name "%b - Emacs" "%b - Emacs"))


;; ========== Line by line scrolling ==========
;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
(setq scroll-step 1)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; check spelling
(defun init-spell-checking ()
  (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
  (add-hook 'text-mode-hook 'flyspell-mode)

  (add-hook 'c-mode 'flyspell-mode)
  ;;(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
  ;;(autoload 'text-mode-flyspell-verify "flyspell" "" t)
  ;;(flyspell-mode 1)
  (global-set-key [backtab] 'ispell-complete-word)
  (setq-default ispell-program-name "aspell")
  (setq flyspell-delay '3))
(init-spell-checking)

;; WoMan
(defun my-woman-pre-format-fn ()
  "Function added to `woman-pre-format-hook'."
;;  (copy-face 'my-Man-overstrike-face 'woman-bold-face)
;;  (copy-face 'my-Man-underline-face 'woman-italic-face)
  (face-spec-set 'woman-addition-face '((t (:foreground "orange" :background "pink"))))
  (face-spec-set 'woman-unknown-face  '((t (:foreground "cyan" :background "pink")))))

(add-hook 'woman-pre-format-hook 'my-woman-pre-format-fn)

;; So that each instance will pop up a new frame.
;; Maybe `special-display-regexps' would be better?
(add-hook 'woman-post-format-hook (lambda () (setq woman-frame nil)))


;(defun get-user-mail-address ()
;  user-mail-address
;)
;(get-user-mail-address)

;; Snippets
(defun init-msf-abbrev ()
  "Set msf-abbrev configuration."
  (require 'msf-abbrev)
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)
  (setq msf-abbrev-root "~/emacs/abb")
  (global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
  (global-set-key (kbd "C-c s") 'abbrev-mode)
  (global-set-key (kbd "C-x a u") 'unexpand-abbrev))
(when (locate-library "msf-abbrev")
  (init-msf-abbrev))

;; dvc - distributed version control system incldes support for hg
(defun init-dvc ()
  "Set dvc configuration."
  (add-to-list 'load-path "~/emacs/deps/dvc")
  (add-to-list 'Info-default-directory-list "/usr/local/share/info/")
  (require 'dvc-autoloads))
(when (file-exists-p "~/emacs/deps/dvc")
  (init-dvc))

; disable vc (version control) module as we have DVC (distributed version control) installed
;(setq vc-handled-backends nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; internal functions
(defun generate-tag-table (dir)
  "Generate tag tables under current directory(Linux)."
  (interactive "DGnerate tags in: ")
  (setq exp
;;        (read-from-minibuffer "suffix: " "*.[chCH]*"))
        (read-from-minibuffer "suffix: " "*.[eh]rl"))
  (with-temp-buffer
    (shell-command
     (concat "find " dir " -name \"" exp "\" -print | xargs etags -o " dir "/TAGS -")
     ))
  (add-to-list 'tags-table-list dir))

;; for changing default compile command uncoment next line
;; (defvar compile-command "scons")
;; OR place next lines into stub Makefile
;; main:
;;    * scons
;;; SCons builds into a 'build' subdir, but we want to find the errors
;;; in the regular source dir.  So we remove build/XXX/YYY/{dbg,final}/ from the
;;; filenames.
(defun process-error-filename (filename)
  (let ((case-fold-search t))
    (setq f (replace-regexp-in-string
             "[Ss]?[Bb]uild[\\/].*\\(final\\|dbg\\)[^\\/]*[\\/]" "" filename))
    (cond ((file-exists-p f)
           f)
          (t filename))))

(setq compilation-parse-errors-filename-function 'process-error-filename)

(when (file-exists-p "~/emacs/deps/elisp")
    (add-to-list 'load-path "~/emacs/deps/elisp"))

;; Desktop mode
; save sessions
;(load "desktop")
;(desktop-save-mode t)
(defun close-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))


;; Save temp file in different directory
(defun init-backups ()
  ;; Put backup files (ie foo~) in ~/emacs/.
  (setq backup-directory "~/emacs/backups")
  (unless (file-exists-p backup-directory)
    (make-directory backup-directory))
  (setq backup-directory-alist
        `((".*" . , backup-directory)))
  (setq backup-by-copying t)
  ;; If backup-by-copying is too slow
  ;; (setq backup-by-copying-when-linked t)
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  ;; Put autosave files (ie #foo#) in ~/emacs/autosaves
  (setq autosaves-directory "~/emacs/autosaves")
  (unless (file-exists-p autosaves-directory)
    (make-directory autosaves-directory))
  (setq auto-save-file-name-transforms
    `((".*" , autosaves-directory t)))
)

(init-backups)

(defun init-color-theme ()

  (add-to-list 'load-path "~/emacs/deps/color-theme")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-bharadwaj-slate))

(when (file-exists-p "~/emacs/deps/color-theme")
  (init-color-theme))

;; Include additional configuration file from current directory
;(when (file-exists-p (concat default-directory ".emacs.el"))
;  (load (concat default-directory ".emacs.el")))

;; =================================================
;; Modes specific configuration
;; ----------------------------

;; C & C++ & Java specific
(when (file-exists-p "~/emacs/etc/c.el")
  (load "~/emacs/etc/c.el"))

;;; graphwiz mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "graphviz-dot Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;;; plantUML mode
(setq plantuml-jar-path (expand-file-name "~/emacs/deps/plantuml.jar"))
(when (and (file-exists-p "~/emacs/deps/plantuml-mode.el")
           (file-exists-p plantuml-jar-path))
  (autoload 'plantuml-mode "plantuml-mode" "plantUML Editing Mode" t))

;; erlang specific
(when (file-exists-p "~/emacs/etc/erlang.el")
  (load "~/emacs/etc/erlang.el"))

;; SCONS
 (setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
 (setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(when (file-exists-p "~/emacs/etc/track.el")
  (load "~/emacs/etc/track.el"))

(when (file-exists-p "~/emacs/etc/java.el")
  (load "~/emacs/etc/java.el"))

(when (file-exists-p "~/emacs/deps/multiple-cursors.el")
  (add-to-list 'load-path "~/emacs/deps/multiple-cursors.el")
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key [(control shift up)] 'mc/mark-previous-like-this)
  (global-set-key [(control shift down)] 'mc/mark-next-like-this)
)

(when (file-exists-p "~/emacs/deps/auto-complete")
  (add-to-list 'load-path "~/emacs/deps/auto-complete")
  (add-to-list 'load-path "~/emacs/deps/auto-complete/lib/popup/")
  (require 'auto-complete)
  (add-hook 'elixir-mode-hook 'auto-complete-mode)
  (add-hook 'java-mode-hook 'auto-complete-mode))

