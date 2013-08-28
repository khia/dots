all: emacs/ ~/emacs ~/.emacs.el

emacs/deps/plantuml.jar:
	@cd emacs/deps/ && wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download

emacs/elixir:
	git clone git://github.com/secondplanet/elixir-mode emacs/elixir

emacs/: emacs/*.el
	emacs --batch --eval '(byte-recompile-directory "$(@D)")'

~/emacs:
	ln -s `pwd`/emacs ${HOME}/emacs

~/.emacs.el:
	ln -s `pwd`/.emacs.el ${HOME}/.emacs.el