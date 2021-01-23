FROM ubuntu

RUN apt update
RUN apt-get install -y build-essential diffutils vim wget curl

RUN ln -s bash /bin/sh.bash && mv /bin/sh.bash /bin/sh

RUN mkdir downloads
RUN cd downloads; wget https://www.nasm.us/pub/nasm/releasebuilds/2.15.05/nasm-2.15.05.tar.xz && tar -xf nasm-2.15.05.tar.xz && cd nasm-2.15.05 && ./configure --prefix=/usr && make && make install

RUN mkdir /work
WORKDIR /work

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain nightly
ENV PATH=${PATH}:/root/.cargo/bin