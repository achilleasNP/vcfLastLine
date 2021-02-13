FROM haskell:latest
RUN useradd -ms /bin/bash newuser
USER newuser
WORKDIR /home/newuser
