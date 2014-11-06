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
$ cabal install --avoid-reinstalls
$ .cabal-sandbox/bin/ledger
# http://localhost:8080
```

## Configuration

To configure Haskell, create a [Configurator][3] file.

``` cfg
# tmp/ledger.cfg
ledger { key = "secret" }
warp { port = 8888 }
acid-state { directory = "state/ledger" }
```

``` sh
$ .cabal-sandbox/bin/ledger tmp/ledger.cfg
# http://localhost:8888/#secret
```

For a complete list of options, check out [the default configuration][4].

[1]: https://github.com/tfausak/ledger
[2]: https://www.haskell.org/platform/
[3]: https://github.com/bos/configurator
[4]: data/ledger.cfg
