{ sources ? import ../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
, withHoogle ? false
}:

let

  deps = with pkgs; [
    which
    # ...
  ];

  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      # haskellPackage1 = haskellPackagesNew.callPackage ./nix/myPackage1.nix { };
      # haskellPackage2 = haskellPackagesNew.callPackage ./nix/myPackage2.nix { };
      # ...
    };
  };

  buildExports = ''
    export LC_ALL=C.UTF-8
    export GHC_BASE=$(which ghc | cut -d '/' -f-4)
  '';

  drv1 = haskellPackages.callCabal2nix "asterix" ./. { };

  drv = drv1.overrideDerivation (oldAttrs: {
      src = builtins.filterSource
        (path: type:
          (type != "directory" || baseNameOf path != ".git")
          && (type != "symlink" || baseNameOf path != "result"))
        ./.;
      preBuild = buildExports;
      buildInputs = oldAttrs.buildInputs ++ deps;
  });

  env = haskellPackages.shellFor {
    packages = p: with p; [
      drv
    ];

    buildInputs = with haskellPackages; deps ++ [
      niv
      pkgs.cacert # needed for niv
      pkgs.nix    # needed for niv
      cabal-install
      pkgs.ghcid
      pkgs.cabal2nix
      # haskell-language-server
      # fast-tags
      # haskell-debug-adapter
      # ghci-dap
    ];

    withHoogle = withHoogle;

    shellHook = buildExports;
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv
