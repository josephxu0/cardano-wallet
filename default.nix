############################################################################
#
# Cardano Wallet Nix build
#
# Derivation attributes of this file can be build with "nix-build -A ..."
# Discover attribute names using tab-completion in your shell.
#
# Interesting top-level attributes:
#
#   - cardano-wallet-jormungandr - cli executable
#   - cardano-wallet-byron - cli executable
#   - cardano-wallet-shelley - cli executable
#   - tests - attrset of test-suite executables
#     - cardano-wallet-core.unit
#     - cardano-wallet-jormungandr.jormungandr-integration
#     - cardano-wallet-byron.integration
#     - etc (layout is PACKAGE.COMPONENT)
#   - checks - attrset of test-suite results
#     - cardano-wallet-core.unit
#     - cardano-wallet-jormungandr.jormungandr-integration
#     - cardano-wallet-byron.integration
#     - etc
#   - benchmarks - attret of benchmark executables
#     - cardano-wallet-core.db
#     - cardano-wallet-jormungandr.latency
#     - cardano-wallet-byron.restore
#     - etc
#   - migration-tests - tests db migrations from previous versions
#   - dockerImage - tarballs of the docker images
#     - jormungandr
#     - byron
#     - shelley
#   - shell - imported by shell.nix
#   - haskellPackages - a Haskell.nix package set of all packages and their dependencies
#     - cardano-wallet-core.components.library
#     - etc (layout is PACKAGE-NAME.components.COMPONENT-TYPE.COMPONENT-NAME)
#
# The attributes of this file are imported by the Hydra jobset and
# mapped into the layout TARGET-SYSTEM.ATTR-PATH.BUILD-SYSTEM.
# See release.nix for more info about that.
#
# Other documentation:
#   https://github.com/input-output-hk/cardano-wallet/wiki/Building#nix-build
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import pinned Nixpkgs with iohk-nix and Haskell.nix overlays
, pkgs ? import ./nix/default.nix { inherit system crossSystem config sourcesOverride; }
# Use this git revision for stamping executables
, gitrev ? pkgs.commonLib.commitIdFromGitRepoOrZero ./.git
# Use this to reference local sources rather than the niv pinned versions (see nix/default.nix)
, sourcesOverride ? {}
# GitHub PR number (as a string), set when building a Hydra PR jobset.
, pr ? null
}:

# commonLib includes iohk-nix utilities, our util.nix and nixpkgs lib.
with pkgs; with commonLib; with pkgs.haskell-nix.haskellLib;

let
  src = cleanSourceWith {
    src = pkgs.haskell-nix.cleanSourceHaskell { src = ./.; };
    name = "cardano-wallet-src";
    filter = removeSocketFilesFilter;
  };

  buildHaskellPackages = args: import ./nix/haskell.nix ({
    inherit config lib stdenv pkgs buildPackages;
    inherit (pkgs) haskell-nix;
    inherit src pr gitrev;
  } // args);
  haskellPackages = buildHaskellPackages {};
  profiledHaskellPackages = buildHaskellPackages { profiling = true; };

  getPackageChecks = mapAttrs (_: package: package.checks);

  self = {
    inherit pkgs commonLib src haskellPackages profiledHaskellPackages;
    # Jormungandr
    inherit (jmPkgs) jormungandr jormungandr-cli;
    # expose cardano-node, so daedalus can ship it without needing to pin cardano-node
    inherit (pkgs) cardano-node cardano-cli;
    inherit (haskellPackages.cardano-wallet-core.identifier) version;
    # expose db-converter, so daedalus can ship it without needing to pin a ouroborus-network rev
    inherit (haskellPackages.ouroboros-consensus-byron.components.exes) db-converter;
    # adrestia tool belt
    inherit (haskellPackages.bech32.components.exes) bech32;
    inherit (haskellPackages.cardano-addresses.components.exes) cardano-address;
    inherit (haskellPackages.cardano-transactions.components.exes) cardano-tx;

    cardano-wallet-jormungandr = import ./nix/package-jormungandr.nix {
      inherit (haskellPackages.cardano-wallet-jormungandr.components.exes)
        cardano-wallet-jormungandr;
      inherit pkgs jmPkgs gitrev;
      haskellBuildUtils = haskellBuildUtils.package;
    };

    cardano-wallet-byron = import ./nix/package-cardano-node.nix {
      inherit pkgs gitrev;
      haskellBuildUtils = haskellBuildUtils.package;
      exe = haskellPackages.cardano-wallet-byron.components.exes.cardano-wallet-byron;
      inherit (self) cardano-node;
    };

    cardano-wallet-shelley = import ./nix/package-cardano-node.nix {
      inherit pkgs gitrev;
      haskellBuildUtils = haskellBuildUtils.package;
      exe = haskellPackages.cardano-wallet-shelley.components.exes.cardano-wallet-shelley;
      inherit (self) cardano-node;
    };

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isProjectPackage haskellPackages;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (selectProjectPackages haskellPackages));
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isProjectPackage haskellPackages;

    dockerImage = let
      mkDockerImage = backend: exe: pkgs.callPackage ./nix/docker.nix { inherit backend exe; };
    in recurseIntoAttrs (mapAttrs mkDockerImage {
      jormungandr = self.cardano-wallet-jormungandr;
      byron = self.cardano-wallet-byron;
      shelley = self.cardano-wallet-shelley;
    });

    shell = haskellPackages.shellFor {
      name = "cardano-wallet-shell";
      packages = ps: attrValues (selectProjectPackages ps);
      buildInputs = (with self; [
          jormungandr
          jormungandr-cli
          cardano-node
          cardano-cli
          cardano-address
          cardano-tx
          bech32
        ]) ++ (with pkgs; [
          niv
          pkgconfig
          python3Packages.openapi-spec-validator
          ruby
          sqlite-interactive
          yq
        ]);
      tools = {
        cabal = "3.2.0.0";
        ghcid = "0.8.7";
        ghcide = "0.2.0";
        hlint = "3.1.6";
        stylish-haskell = "0.11.0.0";
        weeder = "1.0.9";
      };
      CARDANO_NODE_CONFIGS = cardano-node.deployments;
      meta.platforms = lib.platforms.unix;
      shellHook = ''
        setup_completion() {
          local p
          for p in $buildInputs; do
            if [ -d "$p/share/bash-completion" ]; then
              addToSearchPath XDG_DATA_DIRS "$p/share"
            fi
          done
        }
        setup_completion
      '';
    };
    stackShell = import ./nix/stack-shell.nix {
      walletPackages = self;
    };
    # This is the ./nix/regenerate.sh script. Put it here so that it's
    # built and cached on CI.
    inherit stackNixRegenerate;
    # This attribute ensures that every single derivation required for
    # evaluation of the haskell package set is built and cached on CI.
    inherit (pkgs.haskell-nix) haskellNixRoots;
  };

in
  self
