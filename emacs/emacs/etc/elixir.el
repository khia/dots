
(defun init-elixir ()
  (message "Initializing elixir")
  ;;(setq indent-tabs-mode nil)
  ;;(setq-default tab-width 2)
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  ;; (add-hook 'elixir-mode-hook 'flyspell-mode)
  ;; (add-hook 'elixir-mode 'flyspell-prog-mode 1)
  (add-hook 'elixir-mode 'delete-trailing-whitespace-hook))

(when (file-exists-p "~/emacs/deps/elixir/elixir-mode.el")
   (add-to-list 'load-path "~/emacs/deps/elixir")
    (require 'elixir-mode)
   (add-hook 'elixir-mode-hook 'init-elixir))



