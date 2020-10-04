FROM alpine as builder

RUN apk update && \
    apk add bash git alpine-sdk wget sqlite

RUN apk add z3

RUN apk add racket --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing

RUN apk add glib fontconfig cairo pango jpeg sqlite
RUN raco pkg install --no-docs --deps search-auto typed-racket
RUN mv /usr/lib/libz3.so.4.8 /usr/lib/libz3.so
ENV Z3_LIB /usr/lib
RUN raco pkg install --no-docs --deps search-auto z3

WORKDIR /home
RUN git clone https://github.com/maciejpirog/grzb.git grzb-devel
RUN raco exe -o grzb grzb-devel/src/front-terminal/main.rkt

# # #

FROM alpine

COPY --from=builder /home/grzb /usr/local/bin
COPY --from=builder /usr/lib/libz3.so /usr/lib
COPY --from=builder /usr/bin/racket /usr/bin
COPY --from=builder /usr/lib/libracket3m-7.7.so /usr/lib
COPY --from=builder /lib/libucontext.so.0 /lib
COPY --from=builder /usr/lib/libffi.so.7 /usr/lib
COPY --from=builder /usr/lib/libstdc++.so.6 /usr/lib
COPY --from=builder /usr/lib/libgcc_s.so.1 /usr/lib
ENV Z3_LIB /usr/lib

WORKDIR /home

ENTRYPOINT [ "grzb" ]

# # #

LABEL maintainer="maciej.adam.pirog@gmail.com"
LABEL version="0.1.0"
LABEL description="A While verifier powered by Z3"