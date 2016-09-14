OPAM_PKGS="ocamlfind ounit"

if [ -f "$HOME/.opam/config"]; then
  opam update
  opam upgrade
else
  opam init
fi

opam install $OPAM_PKGS
