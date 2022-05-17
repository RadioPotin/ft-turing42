FROM debian:unstable as init-opam

# --- # SETUP PACKAGES

RUN apt update -yqq
RUN apt upgrade -yqq
RUN DEBIAN_FRONTEND=noninteractive apt install git apt-utils pkg-config make m4 gcc patch unzip bubblewrap -yqq
RUN apt install opam -yqq

# --- # INSTALL AND INIT OPAM

RUN groupadd -r user && useradd --no-log-init -r -m -g user user
RUN usermod -a -G root,user user
USER user
RUN opam init --bare --disable-sandboxing -y
RUN opam switch create rendu ocaml-base-compiler.4.12.0
RUN opam update
RUN eval $(opam env --switch=rendu)

# --- # installing project

WORKDIR /home/user
COPY . .
RUN opam install . --deps-only --y --with-test --with-doc
RUN eval $(opam env --switch=rendu)
RUN opam exec -- dune build @install
RUN opam exec -- dune install
RUN opam exec -- dune runtest
RUN opam exec -- dune build @all
