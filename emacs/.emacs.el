;; self check and compile
(defun self_check_and_compile! (a b)
  (cond
   ((file-newer-than-file-p a b)
    (let ((mode-line-format "*** Recompiling FROM ***"))
      (yow)
      (sit-for 1)
      (byte-compile-file a)
      (message "TO recompiled --- reloading..."))
    (load b t t t)
    )))
(self_check_and_compile! "~/.emacs.el" "~/.emacs.elc")

;; Marmalade
(require 'package)
(add-to-list 'package-archives
    '("marmalade" . "http://marmalade-repo.org/packages/")
)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/")
)

;; update path for problematic packages (autocomplete)
;;(let ((default-directory "~/.emacs.d/elpa"))
;;  (normal-top-level-add-subdirs-to-load-path))

(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defvar my-packages
  '(
    color-theme
    auto-complete
    multiple-cursors
    elixir-mode
    elixir-mix
    erlang
    graphviz-dot-mode
    plantuml-mode
    undo-tree
    dockerfile-mode
    auto-complete-nxml
    rust-mode
    coffee-mode
    python-mode
    helm-dash
    markdown-mode
    markdown-mode+
    w3m
    editor-config
  )
)

(defun bootstap-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
        my-packages))

;; Disable backups
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Enable server-mode
;;(setq server-host "0.0.0.0")
;;(setq server-port "4242")
;;(setq server-use-tcp t)
;;(server-start)

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;; configure multiple-cursors
(after 'multiple-cursors-autoloads
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-<return>") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-M-=") 'mc/insert-numbers)
  (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key [(control shift up)] 'mc/mark-previous-like-this)
  (global-set-key [(control shift down)] 'mc/mark-next-like-this)
)

;; configure color-theme
;; (after 'color-theme
;;  (color-theme-initialize)
;;  (color-theme-bharadwaj-slate)
;;  (init-color-theme)
;;)

;; configure auto-complete
;;(after 'auto-complete
;;  (add-hook 'elixir-mode-hook 'auto-complete-mode)
;;)

(global-linum-mode 1)

;;;; undo-tree
(after 'undo-tree-autoloads
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

;; Look & Feel
(setq inhibit-startup-message t)

(setq visible-bell t)
;; Highlight the current line
(global-hl-line-mode 1)
;; Show paren
(show-paren-mode 1)
;; Status line
(setq-default column-number-mode t)
(setq-default line-number-mode t)
;; Window size
;;(enlarge-window 25)
;;(enlarge-window-horizontally 80)

;; filename in the window title
;;(setq frame-title-format '(buffer-file-name "%b - Emacs" "%b - Emacs"))
(setq frame-title-format '
      (buffer-file-name "%f - Emacs"
                        (dired-directory dired-directory "%b")))

;; ========== Line by line scrolling ==========
;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
(setq scroll-step 1)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; useful functions
(defun close-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

(defun delete-trailing-whitespace-hook ()
  (message "delete-trailing-whitespace-hook")
    (setq show-trailing-whitespace t)
    ;;(post-command-hook 'before-save-hook)
    (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Key bindings
(global-set-key (kbd "C-x RET") 'compile)
(global-set-key "\M-\C-g" 'goto-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;;


(after 'color-theme
  (color-theme-initialize)
  ;;(color-theme-bharadwaj-slate)
  (load-theme 'bharadwaj-slate t)
)

(defun auto-complete-init ()
  (message "Initializing auto-complete")
  (require 'auto-complete-config)
  (after 'auto-complete-config

    ;; dirty fix for having AC everywhere
    (defun auto-complete-mode-maybe ()
      "No maybe for you. Only AC!"
      (unless (minibufferp (current-buffer))
        (auto-complete-mode 1)))
  (global-auto-complete-mode t)))

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;; do things after package initialization
  (message "Initializing packages")
  (auto-complete-init)

  (setq indent-tabs-mode nil)
  (global-linum-mode t)
  (delete-trailing-whitespace-hook)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes (quote ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fci-rule-color "#073642")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198") (120 . "#268bd2") (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16") (220 . "#b58900") (240 . "#859900") (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; helm-dash doesn't support non interactive installation
;; Use helm-dash-install-docset to install whatever needed


(after 'helm-dash
  (defun elixir-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Elixir")))
  (add-hook 'elixir-mode-hook 'elixir-doc)
)

(after 'w3m
  (setq w3m-default-display-inline-images t)
)
