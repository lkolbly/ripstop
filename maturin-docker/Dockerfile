FROM quay.io/pypa/manylinux2014_x86_64

RUN curl https://static.rust-lang.org/dist/rust-1.66.0-x86_64-unknown-linux-gnu.tar.gz | gunzip -c | tar xv
RUN ./rust-1.66.0-x86_64-unknown-linux-gnu/install.sh

COPY install-maturin.sh /
RUN sh /install-maturin.sh

# Download the registry
RUN cargo install cargo-tree

WORKDIR /work

SHELL ["/bin/bash", "-c", "-l"]
