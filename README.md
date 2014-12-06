# [Ledger][1]

Watch your money fly away.

- [Installation](#installation)
- [Configuration](#configuration)

## Installation

To install Ledger, you'll need [The Haskell Platform][2] version 2014.2.0.0.

``` sh
$ git clone https://github.com/tfausak/ledger.git
$ cd ledger
$ cabal update
$ cabal sandbox init
$ cabal install
$ cabal run
# http://localhost:8080
```

## Configuration

To configure Ledger, create a [Configurator][3] file.

``` cfg
# tmp/ledger.cfg
warp { port = "8888" }
acid-state { directory = "state/ledger" }
```

``` sh
$ cabal run tmp/ledger.cfg
# http://localhost:8888
```

For a complete list of options, check out [the default configuration][4].

[1]: https://github.com/tfausak/ledger
[2]: https://www.haskell.org/platform/
[3]: https://github.com/bos/configurator
[4]: data/ledger.cfg
