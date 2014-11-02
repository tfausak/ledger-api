FROM haskell:7.8
MAINTAINER taylorfausak
EXPOSE 8080
RUN cabal update
ADD ledger.cabal /ledger/ledger.cabal
RUN cd /ledger && cabal sandbox init
RUN cd /ledger && cabal install --only-dependencies --jobs
ADD . /ledger
RUN cd /ledger && cabal install --jobs
WORKDIR /ledger
CMD .cabal-sandbox/bin/ledger
