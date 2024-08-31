FROM ocaml/opam:debian-ocaml-5.1

RUN sudo apt-get update -y && \
    sudo apt-get install -y m4 pkg-config libev-dev libssl-dev libgmp-dev

RUN opam install conf-libssl -y

WORKDIR /home/opam/bluesky-bot

RUN opam install dune lwt cohttp-lwt-unix yojson redis-lwt logs fmt ptime lwt_ppx tls tls-lwt conduit-lwt-unix ssl ipaddr -y && \
    opam reinstall conduit conduit-lwt conduit-lwt-unix cohttp cohttp-lwt cohttp-lwt-unix tls tls-lwt ssl ipaddr -y

RUN eval $(opam env)

COPY --chown=opam:opam . .

RUN eval $(opam env) && opam install . --deps-only -y

RUN eval $(opam env) && dune build @install

CMD ["dune", "exec", "src/main.exe"]