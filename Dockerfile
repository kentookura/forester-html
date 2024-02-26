FROM ocaml/opam:debian-12-ocaml-5.1 as build
WORKDIR /build

# Install dependencies.
RUN sudo apt-get update
RUN sudo apt-get install -y libev-dev libpq-dev pkg-config libcurl4-gnutls-dev libsqlite3-dev zlib1g-dev
ADD . .
RUN opam install --deps-only .

RUN mkdir -p _build
# Build project.
RUN opam exec -- dune build



FROM debian:stable-20231120-slim as run

RUN apt-get update
RUN apt-get install -y libev4 libpq5 libssl3

COPY --from=build /build/_build/install/default/bin/dream_auth /bin/postgres

ENTRYPOINT /bin/postgres
