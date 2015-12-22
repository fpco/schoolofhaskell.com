#-*- mode: conf; -*-

FROM fpco/ubuntu-with-libgmp:14.04
RUN apt-get update \
 && apt-get install -y libpq5 openssh-client \
 && apt-get clean \
 && ln -s config/client_session_key.aes client_session_key.aes
