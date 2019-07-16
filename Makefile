EMACS=`which  emacs`

all: install

install:
	@echo ":::: Installing packages ::::"
	@$(EMACS) --script init.el
	@echo

update_git:
	@echo ":::: Updating init files ::::"
	@git fetch
	@git pull origin master
	@echo

update_packages:
	@echo ":::: Updating packages ::::"
	@$(EMACS) --script update.el
	@echo

update: update_git install update_packages
