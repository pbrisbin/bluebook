PANDOC ?= stack exec pandoc --
MANPREFIX ?= $(HOME)/.local/share/man

.PHONY: doc.install
doc.install:
	mkdir -p "$(MANPREFIX)"/man1
	$(PANDOC) \
	  --standalone README.md \
	  --to man > "$(MANPREFIX)"/man1/bluebook.1
