FROM fedora

RUN dnf install -y gcc nasm diffutils

RUN  mkdir work
WORKDIR /work

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain nightly
ENV PATH=${PATH}:/root/.cargo/bin