final: prev:

let
  mkCloudy = final: hfinal:
    let
      filesToIgnore = [
        ".git"
        ".github"
        ".stack-work"
        ".travis.yml"
        "cabal.project"
        "default.nix"
        "flake.lock"
        "flake.nix"
        "nix"
        "result"
        "shell.nix"
        "stack-nightly.yaml"
        "stack.yaml"
      ];

      src =
        builtins.path {
          name = "cloudy-src";
          path = ./..;
          filter = path: type:
            with final.lib;
            ! elem (baseNameOf path) filesToIgnore &&
            ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
        };

      extraCabal2nixOptions = "";
    in
    hfinal.callCabal2nixWithOptions "cloudy" src extraCabal2nixOptions {};
in
{
  ###############################
  ## Haskell package overrides ##
  ###############################

  cloudy-haskell =
    let
      myHaskellOverride = oldAttrs: {
        overrides =
          final.lib.composeExtensions
            (oldAttrs.overrides or (_: _: {}))
            (hfinal: hprev: {
              cloudy = mkCloudy final hfinal;
            });
      };
    in
    prev.haskell // {
      packages = prev.haskell.packages // {
        ${final.cloudy-ghc-version} =
          prev.haskell.packages.${final.cloudy-ghc-version}.override
            myHaskellOverride;

        native-bignum = prev.haskell.packages.native-bignum // {
          ${final.cloudy-static-ghc-version} =
            prev.haskell.packages.native-bignum.${final.cloudy-static-ghc-version}.override
              myHaskellOverride;
        };
      };
    };

  cloudy-ghc-version-short = "966";

  cloudy-ghc-version = "ghc" + final.cloudy-ghc-version-short;

  cloudy-haskell-pkg-set = final.cloudy-haskell.packages.${final.cloudy-ghc-version};

  cloudy = final.cloudy-haskell-pkg-set.cloudy;

  cloudy-just-exe =
    final.lib.pipe
      final.cloudy
      [
        # The code-path that generates the optparse-applicative completions uses
        # the HOME directory, so that must be set in order to generate completions.
        # https://github.com/cdepillabout/cloudy/issues/10
        ( final.haskell.lib.compose.overrideCabal (oldAttrs: {
            postInstall = ''
                export HOME=$TMPDIR
              '' + (oldAttrs.postInstall or "");
          })
        )
        (final.cloudy-haskell-pkg-set.generateOptparseApplicativeCompletions ["cloudy"])
        final.haskell.lib.justStaticExecutables
      ];

  cloudy-shell = final.cloudy-haskell-pkg-set.shellFor {
    packages = hpkgs: [ hpkgs.cloudy ];
  };

  my-haskell-language-server =
    final.haskell-language-server.override {
      supportedGhcVersions = [ final.cloudy-ghc-version-short ];
    };

  dev-shell = final.mkShell {
    nativeBuildInputs = [
      final.cabal-install
      final.my-haskell-language-server
      final.sqlite-interactive
    ];
    inputsFrom = [
      final.cloudy-shell
    ];
    # These environment variables are important. Without these,
    # doctest doesn't pick up nix's version of ghc, and will fail
    # claiming it can't find your dependencies
    shellHook = ''
      export NIX_GHC="${final.cloudy-shell.NIX_GHC}"
      export NIX_GHC_LIBDIR="${final.cloudy-shell.NIX_GHC_LIBDIR}"
    '';
  };

  ##############################
  ## Statically-linked builds ##
  ##############################

  # Static builds only work on ghc94, not ghc96+ :-(
  cloudy-static-ghc-version-short = "948";

  cloudy-static-ghc-version = "ghc" + final.cloudy-static-ghc-version-short;

  cloudy-static-haskell-pkg-set = final.pkgsStatic.cloudy-haskell.packages.native-bignum.${final.cloudy-static-ghc-version};

  cloudy-static = final.cloudy-static-haskell-pkg-set.cloudy;

  cloudy-static-just-exe =
    final.lib.pipe
      final.cloudy-static
      [
        ( final.haskell.lib.compose.overrideCabal (oldAttrs: {
            postInstall = ''
                export HOME=$TMPDIR
              '' + (oldAttrs.postInstall or "");
          })
        )
        (final.cloudy-static-haskell-pkg-set.generateOptparseApplicativeCompletions ["cloudy"])
        final.haskell.lib.justStaticExecutables
      ];
}

