# Contributing to DAB

## Development environment

Use `nix-shell` to enter development environment with pinned version
of dependencies and tools, including:
* `Cabal`
* `hlint`
* `stylish-haskell`
* `haskell-language-server`

Since the codebase mostly depends on packages published on Hackage,
it is also possible to work outside `nix-shell` using your system
installed `Cabal`. Don't forget to update to recent Hackage snapshot
using `cabal update`.

## Code formatting and warnings

Code should be formatted using `stylish-haskell`. Helper script to format
all packages - `fmt.sh` is provided in root of the repository.

All packages have `-Werror` enabled so warnings result in compilation error.

## Adding new package

All project packages are listed in `cabal.project`s `packages` stanza. When adding
a new package make sure the project is fully buildable using `nix-build`.

## Overriding Hackage packages

If you need a more recent package version than the one published on Hackage
or a package that is not available on Hackage, you can add it to `cabal.project`
using `source-repository-package`. 

Example:

```
source-repository-package
    type: git
    location: https://github.com/bflyblue/servant-event-stream
    tag: 7dcb2b3b82882b1ea4c7876e7e5e7ba6556e606a
    --sha256: 04h6xv51if2ki8qgk12wc7avfj9jip0ixh92qd5hxmjnfx072m2d
```

### `--sha256`

Since we use `haskell.nix`, `source-repository-package` requires `--sha256` flag.
You can obtain a `sha256` hash using `nix-prefetch-git`. For example:

```command
nix-prefetch-git https://github.com/blockfrost/blockfrost-haskell
```

## Testing

Try to provide tests if applicable. Ideally the tests shouldn't require
internet connection so they work in isolated `nix-build`s.

In case of tests that need require internet/IO/database access, these need to be
guarded by a `Cabal` flag and disabled by default.

## Continuous integration

The project uses GitHub Actions CI to build and test all packages
using `Cabal` and latest packages available on Hackage.

## Submitting a pull request

* Keep pull request focused on one topic
* Mark your pull request as a `Draft` while still working on it
* Provide informative commit messages
* Try to clean-up your commit history before submitting the pull request for review
  * Squash the formatting, whitespace or other minor changes to relevant commits using interactive rebase (`git rebase -i`)
