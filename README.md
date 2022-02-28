# dab

# Usage

```command
nix-shell
export BLOCKFROST_TOKEN_PATH=~/.blockfrost.testnet.token
cabal run chain-watcher-blockfrost

# View the demo on http://localhost:8282/demo/index.html
```

## Building with nix

```command
nix-build -A chain-watcher.components.exes.chain-watcher-blockfrost
```

# Client API

## Register new client

```command
curl -X POST -v localhost:8282/clients/new
$ "cf7f8e9e-36af-476a-ba83-60cb4ac17597"

cid="cf7f8e9e-36af-476a-ba83-60cb4ac17597"
```

## Remove client

```command
curl -X POST -v localhost:8282/clients/remove/${cid}
```

## Subscribe to event

```command
curl -X POST --data '{"tag": "Ping"}' -H "Content-Type: application/json" -v localhost:8282/clients/request/${cid}
```

### Recurring request

```command
curl -X POST --data '{"contents":{"tag":"Ping"},"tag":"Recurring"}' -H "Content-Type: application/json" -v localhost:8282/clients/request/${cid}
```

### Address change request

```command
curl -X POST --data '{"contents":"addr_test1wpzjtlyp6v4qx6gzjm4zc7lsdufw597507y060qhk84vpjsjd625n","tag":"AddressFundsRequest"}' -H "Content-Type: application/json" -v localhost:8282/clients/request/${cid}
```

## Follow server-sent event stream

```command
curl -v -N --http2 -H "Accept:text/event-stream" localhost:8282/sse/${cid}
```
