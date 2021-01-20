FROM rust:alpine3.12
RUN apk add --no-cache gcc musl-dev linux-headers nasm
RUN mkdir -p /test/target
ADD . /test
WORKDIR /test
RUN cargo buil