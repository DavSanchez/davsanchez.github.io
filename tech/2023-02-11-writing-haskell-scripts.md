---
title: "Writing single-file Haskell scripts using Cabal and Nix"
short-title: "Nix Haskell scripts"
date: 2023-02-11
tags:
  - nix
  - haskell
  - tech
  - 🇬🇧
---

This has been already covered in other articles and documentations, but I always forget them and waste so much time looking for the info when I need it, so I'll note it down here for future reference.

## Haskell scripts using Cabal

We can write a simple Haskell script to be run via Cabal by structuring the file in a [certain format](https://cabal.readthedocs.io/en/stable/cabal-commands.html?highlight=env%20cabal#cabal-run):

1. Start with the shebang `#! /usr/bin/env cabal`.
2. Add a `cabal` metadata block inside a Haskell block comment (`{- ... -}`).
3. (Optional) Add a `project` metadata block in the same way if you need it.

The `cabal` block in step 2 allows us to configure the script. It is considered as an executable block in a traditional [Cabal configuration file](https://cabal.readthedocs.io/en/stable/cabal-package.html#package-descriptions). The immediate use for this is to use `build-depends` to include additional libraries required:

```haskell
#! /usr/bin/env cabal
{- cabal:
build-depends: base ^>= 4.16.4
             , text
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn "Hello, world!"
```

Just run this file with `cabal run <path_to_file>` or change the file permissions with `chmod +x <path_to_file>` and call the file directly from the terminal.

## Scripts for any language using `nix-shell`

If you have Nix installed, you can run scripts of this kind (be it for Haskell or any other language actually) through `nix-shell`. This way you can make the script [get its dependencies](https://nixos.org/manual/nix/stable/command-ref/nix-shell.html#use-as-a--interpreter) via Nix:

1. Start with the shebang `#! /usr/bin/env nix-shell`.
2. On a second line, specify the real interpreter and all the packages needed. Some examples include:
    - `#! nix-shell -i runghc -p "ghc.withPackages (pkgs: [ pkgs.text ])"`
    - `#! nix-shell -i python -p python pythonPackages.prettytable`
3. (Optional) Pin the Nixpkgs version used for the script. For example, to use the 22.11 stable branch, or a specific revision:
    - `#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz`
    - `#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/0672315759b3e15e2121365f067c1c8c56bb4722.tar.gz`

Note that, thanks to Nix already including the extra packages, you won't even need Cabal. We use `runghc` as our interpreter. You must use double quotes (`"`) when passing a simple Nix expression in a nix-shell shebang:

```haskell
#! /usr/bin/env nix-shell 
#! nix-shell -i runghc -p "ghc.withPackages (pkgs: [ pkgs.text ])"
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn "Hello, world!"
```

You can also use `ghcid` instead of `runghc` to make the file compile every time you save and show you errors if it fails, for [faster development](https://publish.elbear.com/#Use%20Nix%20to%20make%20experimenting%20with%20Haskell%20easier). I have not tried this yet though.

As you can see, this is really powerful as it allows you to create scripts for any language or tool, even if you have not installed it in your machine!

Normally, `nix-shell` will fetch all the needed dependencies for a derivation and leave you in an interactive shell with all these dependencies available for you to use. Using it as a script works similar and is very convenient.

If you want to get rid of the software fetched by `nix-shell` after you finish using it, you must run [`nix-collect-garbage`](https://nixos.org/manual/nix/stable/command-ref/nix-collect-garbage.html) (if you are not using the scripts and are in an interactive shell instead, leave that shell first with `Ctrl-D`).

## Bonus: Scripts using Stack

Going back to the Haskell specific, if you prefer Stack to Cabal (although Stack already uses Cabal under the hood), a similar script can be created using something like:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-20.10 --package text
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn "Hello, world!"
```

You can check for more options for Stack scripts in the Stack documentation. Note also that using Stack [scripts](https://docs.haskellstack.org/en/stable/scripts/) will trigger the download of the GHC version specified in the `resolver` option.

In my experience, although using Stack helps with reproducibility and had its use when Cabal was not as fine-tuned as nowadays, changing Stack resolvers frequently in projects and scripts will bloat your `~/.stack` directory with many GHC versions.

Currently, I manage my Haskell installations using [GHCup](https://www.haskell.org/ghcup/) on macOS hosts (I find it easier to control GHC/Cabal/HLS versions with `ghcup tui`) or directly with Nix in my NixOS machines.

I'm thinking about trying to get GHCup correctly added to Nixpkgs, as it's [currently not supported](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/174). I would take inspiration from how [rustup](https://rustup.rs) [does it](https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/development/tools/rust/rustup/default.nix#L90) for Rust, but that will be a story for another post...
