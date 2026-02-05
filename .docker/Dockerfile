FROM debian:trixie-slim

# Basic build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    pkg-config \
    libgmp-dev \
    zlib1g-dev \
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
RUN eval $(opam env) && ./INSTALL

# Sanity check Creusot
RUN cargo creusot --help
