FROM debian:trixie-slim

# Basic build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    pkg-config \
    libgmp-dev \
    zlib1g-dev \
    autoconf \
    libzmq3-dev \
    && rm -rf /var/lib/apt/lists/*

# Sanity check
RUN gcc --version && git --version

# Install opam
RUN apt-get update && apt-get install -y \
    opam \
    && rm -rf /var/lib/apt/lists/*

# Initialize opam (no sandboxing in Docker)
RUN opam init --disable-sandboxing --auto-setup -y

# Sanity check opam
RUN opam --version

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Sanity check Rust
RUN rustc --version && cargo --version

# Install SMT solvers and why3 from Debian packages
RUN apt-get update && apt-get install -y \
    z3 \
    cvc5 \
    why3 \
    && rm -rf /var/lib/apt/lists/*

# Sanity check solvers
RUN z3 --version && cvc5 --version && why3 --version

# Install alt-ergo and why3find via opam (get newer alt-ergo)
RUN opam install -y alt-ergo.2.6.0 why3find

# Sanity check opam packages
RUN eval $(opam env) && alt-ergo --version && why3find --version

# Install Creusot
RUN git clone https://github.com/creusot-rs/creusot /opt/creusot
WORKDIR /opt/creusot
RUN git checkout 8417c628d848dd967a42b917acd879b77a94f636
# Skip why3-ide (GUI) - not needed for CI
RUN opam --cli=2.1 var --global in-creusot-ci=true
RUN eval $(opam env) && ./INSTALL

# Sanity check Creusot
RUN cargo creusot --help

# Install Soteria
WORKDIR /opt
RUN git clone https://github.com/soteria-tools/soteria.git
WORKDIR /opt/soteria
RUN opam switch create . --deps-only -y || true
RUN opam install -y mdx alcotest
RUN eval $(opam env) && dune build @install
RUN eval $(opam env) && dune install
# Add Soteria local switch bin to PATH
ENV PATH="/opt/soteria/_opam/bin:${PATH}"

# Sanity check Soteria
RUN soteria-rust --help && soteria-c --help

# Install Obol (Rust frontend for Soteria)
WORKDIR /opt
RUN git clone https://github.com/soteria-tools/obol.git
WORKDIR /opt/obol
RUN eval $(opam env --switch=/opt/soteria) && make build
ENV PATH="/opt/obol/bin:${PATH}"

# Sanity check Obol
RUN obol --help

# Install AFL++ for fuzzing
RUN apt-get update && apt-get install -y \
    afl++ \
    llvm \
    clang \
    && rm -rf /var/lib/apt/lists/*

# Install cargo-afl
RUN cargo install cargo-afl

# Sanity check AFL
RUN afl-fuzz --version && cargo afl --help

WORKDIR /work
