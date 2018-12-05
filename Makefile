EMACS=`which  emacs`

all: install

install:
	$(EMACS) --script init.el

update: install
	$(EMACS) --script update.el
