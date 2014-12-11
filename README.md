# [ledger-api][1]

[![Build Status][2]][3]

Watch your money fly away.

- [Installation](#installation)
- [Configuration](#configuration)
- [Deployment](#deployment)

## Installation

To install ledger-api, you'll need [The Haskell Platform][4] version 2014.2.0.0.

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

To configure ledger-api, create a [Configurator][5] file.

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

## Deployment

To deploy Ledger, create an [OpenShift][7] account.

``` sh
$ rhc app create ledgerapi http://www.accursoft.com/cartridges/yesod.yml
$ cd ledgerapi
$ rhc ssh
```

``` sh
$ echo 'warp {
  host = "$(OPENSHIFT_HASKELL_IP)"
  port = "$(OPENSHIFT_HASKELL_PORT)"
}
acid-state {
  directory = "$(OPENSHIFT_DATA_DIR)/state/ledger-api"
}' > $OPENSHIFT_DATA_DIR/ledger-api.cfg
$ exit
```

``` sh
$ git remote add github https://github.com/tfausak/ledger-api.git
$ git pull github master
$ git push origin github/master:master
```

[1]: https://github.com/tfausak/ledger-api
[2]: https://img.shields.io/travis/tfausak/ledger-api/master.svg?style=flat
[3]: https://travis-ci.org/tfausak/ledger-api
[4]: https://www.haskell.org/platform/
[5]: https://github.com/bos/configurator
[6]: data/ledger-api.cfg
[7]: https://www.openshift.com
