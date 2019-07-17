FROM ocaml/opam2:4.05

RUN sudo apt-get update -qq && \
    sudo apt-get install -y --no-install-recommends \
    m4 && \
    sudo rm -rf /var/lib/apt/lists/*

RUN opam install ocamlbuild menhir
