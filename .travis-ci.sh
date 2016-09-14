OPAM_PKGS="ocamlfind ounit base-bytes base-unix pcre expect fileutils ocamlify ocamlmod omake"

if [ -f "$HOME/.opam/config"]; then
  opam update
  opam upgrade
else
  opam init -y
fi

opam install -y $OPAM_PKGS
