all: emacs/ ~/emacs ~/.emacs.el emacs/etc/priv.el \
	emacs/deps/auto-complete/*.elc \
	emacs/deps/*.elc
#	emacs/deps/elixir/*.elc

emacs/deps/*.el: emacs/deps/multiple-cursors.el

emacs/deps/plantuml.jar:
	@cd emacs/deps/ && wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download

emacs/deps/elixir:
	@git clone https://github.com/elixir-lang/emacs-elixir.git $@

emacs/deps/elixir/*.elc: emacs/deps/elixir
	@cd "$(@D)" && emacs -q --no-site-file --batch -f batch-byte-compile *.el

emacs/deps/flymake:
	git clone https://github.com/illusori/emacs-flymake $@

emacs/deps/flymake/*.elc: emacs/deps/flymake
	@cd "$(@D)" && emacs -q --no-site-file --batch -f batch-byte-compile *.el

emacs/deps/multiple-cursors.el:
	@git clone https://github.com/magnars/multiple-cursors.el.git $@

emacs/deps/auto-complete:
	@git clone https://github.com/auto-complete/auto-complete.git $@
	@cd $@ && git submodule init; git submodule update

emacs/deps/auto-complete/*.elc: emacs/deps/auto-complete
	@cd "$(@D)" && make

emacs/deps/*.elc: emacs/deps/*.el
	@cd "$(@D)" && emacs -q --no-site-file --batch -f batch-byte-compile *.el

~/emacs:
	ln -s `pwd`/emacs ${HOME}/emacs

~/.emacs.el:
	ln -s `pwd`/.emacs.el ${HOME}/.emacs.el

emacs/etc/priv.el:
	echo "Copy ~/emacs/etc/priv.el.example into ~/emacs/etc/priv.el and update accordingly"
	exit 1

