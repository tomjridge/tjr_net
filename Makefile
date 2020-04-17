default: all

-include Makefile.ocaml

all::
	$(DUNE) build bin/receiver.exe bin/sender.exe

run_receiver:
	xterm -hold -e "$(DUNE) exec bin/receiver.exe"

run_sender:
	xterm -hold -e "$(DUNE) exec bin/sender.exe"


docs::

# for auto-completion of Makefile target
clean::
