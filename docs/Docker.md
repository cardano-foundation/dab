# Docker

### Build and load image using Nix

```sh
docker load < $( nix-build -A container --no-out-link )
```

## Running a development image

```sh
docker run \
  --env BLOCKFROST_TOKEN_PATH=/token \
  -v ~/.blockfrost.testnet.token:/token \
  -p 18282:8282 \
  <output of the previous step>
```
