set -a # export all vars
#set -x # debug

libname="tjr_net"
required_packages="lwt,lwt.unix"
description="Network utilities; very rough"

# generic from here ----------------------------------------------------

PKGS="-package $required_packages"
SYNTAX=""

# -thread needed for core
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -thread -g $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -thread -g $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"

mls=`ocamldep -sort -one-line *.ml`
cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"

natives="
"

branch=`git symbolic-ref --short HEAD` 
v=`date +'%F'`
if [ "$branch" = "master" ]; then
    package_name="${libname}"
else 
    package_name="${libname}_${branch}"
fi


function mk_meta() {
cat >META <<EOF
name="$package_name"
description="$description"
version="$v"
requires="$required_packages"
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF
}


function clean() {
    rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa
    rm -f META
}
