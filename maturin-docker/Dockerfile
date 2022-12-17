FROM quay.io/pypa/manylinux2014_x86_64

RUN curl --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY install-maturin.sh /
RUN sh /install-maturin.sh

# Download the registry
RUN . ~/.cargo/env && cargo install cargo-tree

WORKDIR /work

ENV PATH /root/.cargo/bin:$PATH

SHELL ["/bin/bash", "-c", "-l"]

#ENTRYPOINT [ "bash", "-x" ]
