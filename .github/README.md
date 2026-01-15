# David's Personal Website

This repository contains the source code and content for my personal website and blog, where I document my learning journey and share technical insights on Nix, Haskell, Rust, and more.

The site is built using [Emanote](https://emanote.srid.ca/) and hosted on GitHub Pages at [blog.tech.davidslt.es](https://blog.tech.davidslt.es).

## Running using Nix

To start the Emanote live server using Nix:

```sh
# If you using VSCode, you can also: Ctrl+Shift+B
nix run
```

To update Emanote version in flake.nix:

```sh
nix flake update emanote
```

To build the static website via Nix:

```sh
nix build -o ./result
# Then test it:
nix run nixpkgs#nodePackages.live-server -- ./result
```

## GitHub Pages

GitHub Actions CI is responsible for deploying to GitHub Pages. See `.github/workflows/publish.yaml`.
