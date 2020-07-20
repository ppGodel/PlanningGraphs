FROM ubuntu:18.04 AS builder
MAINTAINER Jose Hdz <jhernandez.sld@e.email>
RUN dpkg --add-architecture i386 && \
    apt-get update && \
    apt-get install -y \
            build-essential \
            gcc-multilib \
            g++-multilib
RUN apt-get install -y \
            xz-utils \
            multiarch-support \
            libc6:i386 \
            libncurses5:i386 \
            libstdc++6:i386 \
            flex \
            bison \
            git
RUN mkdir bb_print && \
    mkdir blackbox_planner && \
    git clone https://gitlab.com/ppGodel/BlackBox.git ./blackbox_planner && \
    cd blackbox_planner && \
    make blackbox && \
    cp blackbox /

FROM ubuntu:18.04
COPY --from=builder /blackbox /

RUN dpkg --add-architecture i386 && \
    apt-get update && \
    apt-get install -y \
            build-essential \
            gcc-multilib \
            g++-multilib

ENTRYPOINT ["/blackbox"]
