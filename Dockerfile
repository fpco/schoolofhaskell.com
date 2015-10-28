#-*- mode: conf; -*-

FROM fpco/ubuntu-with-libgmp:14.04
RUN apt-get update \
 && apt-get install -y libpq-dev
 && apt-get clean
