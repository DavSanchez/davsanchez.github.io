---
title: "Installing x86-64 Nix packages on Apple Silicon Macs"
short-title: "x86_64 Nix Packages on M1"
date: 2022-12-04
tags:
  - nix
  - tech
  - 🇬🇧
---

> ## 2023-03-25 EDIT
>
> Thanks to having skimmed through the [`devenv.sh`](https://devenv.sh) documentation and having followed some [related questions raised in GitHub](https://github.com/Misterio77/nix-starter-configs/issues/6), I now have a cleaner way to do this. I have modified the article to reflect it.

When on an Apple Silicon Mac, trying to install a package from Nixpkgs that doesn't include the `aarch64-darwin` platform, even if it has the `x86_64-darwin` one, will probably fail with a message like this:

```sh
$ nix profile install nixpkgs#purescript
error: Package ‘purescript-0.15.6’ in /nix/store/8c75f43ms4brvphrgdy1a8vy5dy1j0si-source/pkgs/development/compilers/purescript/purescript/default.nix:61 is not supported on ‘aarch64-darwin’, refusing to evaluate.

       a) To temporarily allow packages that are unsupported for this system, you can use an environment variable
          for a single invocation of the nix tools.

            $ export NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1

        Note: For `nix shell`, `nix build`, `nix develop` or any other Nix 2.4+
        (Flake) command, `--impure` must be passed in order to read this
        environment variable.

       b) For `nixos-rebuild` you can set
         { nixpkgs.config.allowUnsupportedSystem = true; }
       in configuration.nix to override this.

       c) For `nix-env`, `nix-build`, `nix-shell` or any other Nix command you can add
         { allowUnsupportedSystem = true; }
       to ~/.config/nixpkgs/config.nix.
(use '--show-trace' to show detailed location information)
```

However, Apple Silicon Macs can install Rosetta 2, enabling them to run x86-64 binaries with great performance. So, with this and the power of Nix overlays, we will see how you can allow a Nix-enabled Mac to install `x86_64-darwin` software.

## Installing Rosetta 2 if you do not have it yet

An obvious first step that can be done from your terminal:

```sh
/usr/sbin/softwareupdate --install-rosetta --agree-to-license
```

## Interlude: A very quick primer on Nix overlays

Essentially, overlays allow users to modify or extend the contents of Nixpkgs, for example to add your own derivations (packages) or modify the way some available derivations are built and installed in the target system.

Good resources for how to use overlays are in the [Nixpkgs manual](https://nixos.org/manual/nixpkgs/stable/#chap-overlays) and the [NixOS Wiki](https://nixos.wiki/wiki/Overlays).

What we are going to do, then, is adding the `x86_64-darwin` derivations that we want to an overlay, so it can be installed on our Apple Silicon system, even if the derivation does not have `aarch64-darwin` as an available platform such as in our above example with `purescript`!

### nix-darwin and Nix flakes

This assumes that you are using Nix and nix-darwin to manage your system configuration, and that you are using **flakes** to do so.

To read more on this, you can:

- For nix-darwin: read and follow the [nix-darwin documentation](https://github.com/LnL7/nix-darwin) to install and enable flakes.
- For flakes: you can check the [Wiki entry](https://nixos.wiki/wiki/Flakes) for it. Although flakes are currently an experimental feature, there are many resources for how to use them already and are stable enough to be used without major issues. For a great guide to start doing this including useful comments in the Nix code, check [Misterio77's starter config templates](https://github.com/Misterio77/nix-starter-configs) to guide you. That guide does not use nix-darwin, but you will see it is easy to integrate it.

I recently switched my own Nix config to follow the structure suggested in the above template due to its clarity, so do not hesitate to take a look at [my nix config repository](https://github.com/DavSanchez/nix-dotfiles) and ask if you have any doubts!

The examples also use Home Manager, but that has less of an impact here.

## Adding the overlay directly in `flake.nix`

If you have already dabbled in the gateway drug to Nix that is managing your whole computer config(s) with it, while looking for ways to start with your Mac, perhaps you stumbled upon a gist like [this one](https://gist.github.com/jmatsushita/5c50ef14b4b96cb24ae5268dab613050), which includes an overlay to enable installing x86-64 packages in Apple Silicon Macs.

```nix
{
  inputs = {
    # Package sets
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    # Environment/system management
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs:
    let
      inherit (darwin.lib) darwinSystem;
      inherit (inputs.nixpkgs-unstable.lib)
        attrValues
        makeOverridable
        optionalAttrs
        singleton;
      # Configuration for `nixpkgs`
      nixpkgsConfig = {
        config = { allowUnfree = true; };
        overlays = attrValues self.overlays ++ singleton (
          # Sub in x86 version of packages that don't build on Apple Silicon yet
          final: prev: (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            inherit (final.pkgs-x86)
              # x86-64 packages made available below
              idris2
              nix-index
              niv
              purescript;
          })
        );
      };
    in
    {
      # My nix-darwin configs    
      darwinConfigurations = rec {
        samplesystem = darwinSystem {
          system = "aarch64-darwin";
          modules = attrValues self.darwinModules ++ [
            # Main nix-darwin config
            ./configuration.nix
            # `home-manager` module
            home-manager.darwinModules.home-manager
            {
              nixpkgs = nixpkgsConfig;
              # `home-manager` config
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.sampleuser = import ./home.nix;
            }
          ];
        };
      };
      # Overlays ---------------------------------------------------------------
      # Overlays to add various packages into package set
      overlays = {
        # Overlay useful on Macs with Apple Silicon
        apple-silicon = final: prev:
          optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            # Add access to x86 packages system is running Apple Silicon
            pkgs-x86 = import inputs.nixpkgs-unstable {
              system = "x86_64-darwin";
              inherit (nixpkgsConfig) config;
            };
          };
      };
    };
}
```

While this of course works, the overlay is defined in `flake.nix` itself and requires access to `inputs.nixpkgs`. The resulting file could be a bit unwieldy if you do not fully grok the Nix language, the functions used or the overlays' mechanism. It seems to alter Nixpkgs by defining overlays in two different places (the flake outputs with the `apple-silicon` overlay and a `nixpkgsConfig` variable which in turn changes the `nixpkgs` used in the Home Manager module).

If, however, you want to split the overlay definitions in separate directories like it's done in some popular configs, using a similar setup becomes difficult, as the additional layers of indirection used still depend on both the `nixpkgs` input and the flake outputs.

## A different approach

Suppose that you are using this different directory structure then:

```sh
λ tre --limit 2
.
├── flake.lock
├── flake.nix
├── home-manager        # Home Manager settings
│   ├── user1-home.nix
│   ├── user2-home.nix
│   └── ...
├── hosts               # System configus (including nix-darwin)
│   ├── macbookpro
│   ├── nixos
│   └── ...
├── nixpkgs.nix
├── overlays            # Overlays directory
│   └── default.nix
└── pkgs                # Custom packages directory (see below)
    ├── default.nix
    ├── kcctl.nix
    └── ...
```

### Including your custom packages not pushed to `nixpkgs`

A possible way to use your custom derivations located in `pkgs` would be to include a `nixpkgs.nix` file whose contents pull from the Nixpkgs version used in `flake.lock` (this idea comes again from Misterio77's starter templates):

```nix
# ./nixpkgs.nix

# A nixpkgs instance that is grabbed from the pinned nixpkgs commit in the lock file
# This is useful to avoid using channels when using legacy nix commands
let lock = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked;
in
import (fetchTarball {
  url = "https://github.com/nixos/nixpkgs/archive/${lock.rev}.tar.gz";
  sha256 = lock.narHash;
})
```

Then, this Nixpkgs instance can be used in `pkgs/default.nix` as the Nixpkgs entry point for your custom derivations, hence using the same revision that your configuration flake is using:

```nix
# ./pkgs/default.nix

# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example' or (legacy) 'nix-build -A example'

{ pkgs ? (import ../nixpkgs.nix) { } }: {
  kcctl = pkgs.callPackage ./kcctl.nix { };
  cotp = pkgs.callPackage ./cotp.nix {
    inherit (pkgs.darwin.apple_sdk.frameworks) AppKit;
  };
  # ...
}
```

You can add all your custom packages included in `pkgs/default.nix` to your overlay, so they are available to your whole config:

```nix
# ./overlays/default.nix

# This file defines overlays
{
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs { pkgs = final; };

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
  };
}
```

### Adding `x86_64-darwin` packages to the overlay

Nix overlays allow to easily expose `x86_64-darwin` packages to an `aarch64-darwin` system. If we turn the `overlays.nix` file (which currently defines an attribute set or _attrset_) into a function that accepts the flake inputs, we can just add a new overlay:

```nix
# ./overlays/default

# This file defines overlays
{ inputs, ... }:
{
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs { pkgs = final; };

  rosetta-packages = _final: prev: {
    rosetta =
      if prev.stdenv.isDarwin && prev.stdenv.isAarch64
      then prev.pkgsx86_64Darwin
      else prev;
  };

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = _final: _prev: {
      # example = prev.example.overrideAttrs (oldAttrs: rec {
      # ...
      # });
    };
}
```

What we did here is define a new `rosetta-packages` overlay which contains a `rosetta` attribute. Using this attribute will set nixpkgs to the one from the [`pkgsx86Darwin` bootstrapping stage](https://github.com/NixOS/nixpkgs/pull/161657/files) (i.e. the nix packages collection for the `x86_64-darwin` platform) if the platform is `aarch64-darwin`, and the usual nixpkgs otherwise.

### Making the overlays available

To expose these overlays in our flake, we import the directory in the `overlays` attribute of the flake `outputs`, passing the `inputs` as an argument (remember that our `overlays.nix` is a function!):

```nix
## flake.nix

overlays = import ./overlays { inherit inputs; };
```

Suppose that then we want to install both `purescript` and `kcctl` as system packages to our, say, Home Manager config. If we expose the flake outputs to our user's `homeConfigurations` attribute like this:

```nix
## flake.nix

# macOS systems using nix-darwin
{
  homeConfigurations = {
    "user1" = home-manager.lib.homeManagerConfiguration {
      # Home-manager requires 'pkgs' instance
      pkgs = nixpkgs.legacyPackages.aarch64-darwin; 
      extraSpecialArgs = { inherit inputs outputs; };
      modules = [
        # > Our main home-manager configuration file <
        ./home-manager/user1-home.nix
      ];
    };
}
```

Then we are only need to add the x86-64 Nix packages in our Apple Silicon computer by selecting the package with the `rosetta` attribute.

```nix
## ./home-manager/user1-home.nix

{ inputs, outputs, lib, config, pkgs, ... }: {
  nixpkgs = {
    # You can add overlays here
    overlays = [
      # If you want to use overlays your own flake exports (from overlays dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.rosetta-packages

      # Or overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
  };

  # TODO: Set your username
  home = {
    username = "your-username";
    homeDirectory = "/home/your-username";
  };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  home.packages = with pkgs; [ 
    steam
    # And our x86_64-darwin packages!
    rosetta.kcctl      # This is a custom package defined in ./pkgs/kcctl.nix
    rosetta.purescript # This is a package that is already present in nixpkgs
  ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.05";
}
```

This way, if we are building our config for `aarch64-darwin`, the `rosetta` attribute will select the `x86_64-darwin` packages for us, leaving the normal instance of `nixpkgs` when we build it for other platforms (the same result as if we hadn't used `rosetta.*` in the first place.)

## Conclusions

This approach is extensible enough while maintaining a degree of separation, uncluttering the base `flake.nix` and centering the addition of packages to the overlay mechanism. Nice!

If you want to try this yourself or look at real examples of this in action, do not hesitate to head to [Misterio77's nix-starter-configs](https://github.com/Misterio77/nix-starter-configs) repository, which I can't recommend enough, or check [my own configs](https://github.com/DavSanchez/nix-dotfiles).

Feel free to reach out and ask for any doubts!
