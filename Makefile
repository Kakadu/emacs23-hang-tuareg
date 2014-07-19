.PHONY: all

all:
	echo 'Scroll to the bottom with Page down' && $(MAKE) -C tuareg && emacs tests/tree.ml

