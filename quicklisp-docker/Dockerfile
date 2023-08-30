FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y sbcl curl gnupg
RUN useradd -ms /bin/bash lisp
USER lisp
WORKDIR /home/lisp

# Setup quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
RUN curl -O https://beta.quicklisp.org/release-key.txt
RUN gpg --import release-key.txt
RUN gpg --verify quicklisp.lisp.asc quicklisp.lisp
RUN sbcl --non-interactive \
       --load quicklisp.lisp \
       --eval '(quicklisp-quickstart:install)' \
       --eval '(ql::without-prompting (ql:add-to-init-file))'