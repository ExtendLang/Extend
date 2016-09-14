OPAM_PKGS="ocamlfind ounit base-bytes base-unix pcre expect fileutils ocamlify ocamlmod omake oasis"

if [ -f "$HOME/.opam/config"]; then
  opam update
  opam upgrade
else
  opam init -y -a
fi

eval `opam config env`

opam install -y $OPAM_PKGS

ocaml setup.ml -configure --enable-tests
