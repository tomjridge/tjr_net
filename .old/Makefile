SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV


all: lib
	$(MAKE) -C bin


lib:
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	@echo "NOTE cma contains: $$cmos" # simple check
	$$mk_cma -g -a -o $$libname.cma $$cmos
	$$mk_cmxa -g -a -o $$libname.cmxa $$cmxs
	$(MAKE) install


install:
	-ocamlfind remove $$package_name
	mk_meta
	ocamlfind install $$package_name META *.cmi *.o *.a *.cma *.cmxa *.cmo *.cmx



uninstall:
	ocamlfind remove $$package_name


clean: FORCE
	clean
	$(MAKE) -C bin clean

real_clean: clean


FORCE:
