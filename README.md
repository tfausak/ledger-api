# [Ledger][1]

[![Build Status][2]][3]

Watch your money fly away.

- [Installation](#installation)
- [Configuration](#configuration)

## Installation

To install Ledger, you'll need [The Haskell Platform][4] version 2014.2.0.0.

``` sh
$ git clone https://github.com/tfausak/ledger-api.git
$ cd ledger-api
$ cabal update
$ cabal sandbox init
$ cabal install
$ cabal run
# http://localhost:8080
```

## Configuration

To configure Ledger, create a [Configurator][5] file.

``` cfg
# tmp/ledger-api.cfg
warp { port = "8888" }
acid-state { directory = "state/ledger-api" }
```

``` sh
$ cabal run tmp/ledger-api.cfg
# http://localhost:8888
```

For a complete list of options, check out [the default configuration][6].

[1]: https://github.com/tfausak/ledger-api
[2]: https://img.shields.io/travis/tfausak/ledger-api/master.svg?style=flat
[3]: https://travis-ci.org/tfausak/ledger-api
[4]: https://www.haskell.org/platform/
[5]: https://github.com/bos/configurator
[6]: data/ledger-api.cfg
