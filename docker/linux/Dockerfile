FROM fedora

RUN dnf install -y gcc nasm diffutils

ADD ./ /
WORKDIR /

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH=${PATH}:/root/.cargo/bin
RUN rustup default nightly

RUN cargo build
RUN cargo test
RUN cd ./test; ./test.sh