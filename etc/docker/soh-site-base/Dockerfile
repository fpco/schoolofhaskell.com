# Dockerfile for fpco/soh-site-base
#-*- mode: conf; -*-

FROM ubuntu:14.04

RUN apt-get update \
 && apt-get install -y --no-install-recommends libgmp10 libpq5 openssh-client ca-certificates \
 && apt-get clean

# Symlink client_session_key.aes because soh-site seems to insist on
# looking for the file in the root.
RUN ln -s config/client_session_key.aes client_session_key.aes
