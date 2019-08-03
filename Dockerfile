FROM ocaml/opam2:4.05

RUN sudo apt-get update -qq && \
    sudo apt-get install -y --no-install-recommends \
    python3.7 \
    gdb \
    m4 && \
    sudo rm -rf /var/lib/apt/lists/*

RUN opam install ocamlbuild menhir

RUN sudo ln -s /usr/bin/python3.7 /usr/bin/python3
