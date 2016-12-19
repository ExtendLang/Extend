OPAM_PKGS="ocamlfind ounit oasis llvm.3.8"

llvm-config --version

if [ -f "$HOME/.opam/config"]; then
  opam update
  opam upgrade
else
  opam init -y -a
fi

eval `opam config env`

opam install -y $OPAM_PKGS

ocaml setup.ml -configure --enable-tests
ocaml setup.ml -build

make -C src/stdlib/

ocaml setup.ml -test -no-hygiene
