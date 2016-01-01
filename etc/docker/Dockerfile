# Dockerfile for fpco/soh-site-base
#-*- mode: conf; -*-

FROM fpco/ubuntu-with-libgmp:14.04

RUN apt-get update \
 && apt-get install -y libpq5 openssh-client \
 && apt-get clean

# Symlink client_session_key.aes because soh-site seems to insist on
# looking for the file in the root.
RUN ln -s config/client_session_key.aes client_session_key.aes
