{ sources ? import ../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
, withHoogle ? false
}:

let

  deps = with pkgs; [
    which
    stylish-haskell
    hlint
    # ...
  ];

  code-generator = import ../../code-generator/default.nix { inShell = false; };

  aspecsRef = builtins.fromJSON (builtins.readFile ../../nix/aspecs.json);
    aspecsDir = pkgs.fetchgit {
    url = aspecsRef.url;
    rev = aspecsRef.rev;
    sha256 = aspecsRef.sha256;
  };

  testSpecs = import ../../test-specs/default.nix { inShell = false; };

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
    export PATH=${code-generator}/bin:$PATH
    export ASTERIX_SPECS_REF="git:${aspecsRef.rev}"
    export ASTERIX_SPECS_DATE="${aspecsRef.date}"
    export ASTERIX_SPECS_FILES=$(find ${aspecsDir}/specs/cat* | grep "\.ast")
    export TEST_ASTERIX_SPECS_FILES=$(find ${testSpecs}/* | grep "\.ast")
  '';

  drv1 = haskellPackages.callCabal2nix "libasterix" ./. { };

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
    ];

    withHoogle = withHoogle;

    shellHook = buildExports;
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv

