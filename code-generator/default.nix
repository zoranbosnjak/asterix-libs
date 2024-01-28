{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
, withHoogle ? false
, aspecsRef ? builtins.fromJSON (builtins.readFile ../nix/aspecs.json)
}:

let
  aspecsDir = pkgs.fetchgit {
    url = aspecsRef.url;
    rev = aspecsRef.rev;
    sha256 = aspecsRef.sha256;
  };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      aspecs = haskellPackagesNew.callPackage "${aspecsDir}/tools" {packages=pkgs; inShell=false;};
    };
  };

  buildExports = ''
      export LC_ALL=C.UTF-8
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
      export EXTENSIONS=$(cat .ghci | grep ":set -X" | awk '{print $2}' | xargs)
      export ASTERIX_SPECS=${aspecsDir}
      export SPECS=$(find ${aspecsDir}/specs/cat* | grep "\.ast")
      export TESTSPECS=$(find ${aspecsDir}/specs/test/* | grep "\.ast")
    '';

  drv1 = haskellPackages.callCabal2nix "code-generator" ./. { };

  drv = drv1.overrideDerivation (oldAttrs: {
    src = builtins.filterSource
      (path: type:
        (type != "directory" || baseNameOf path != ".git")
        && (type != "symlink" || baseNameOf path != "result"))
        ./.;
    preBuild = buildExports;
  });

  env = haskellPackages.shellFor {
    packages = p: with p; [
      drv
    ];

    buildInputs = [
      pkgs.ghcid
      pkgs.which
      pkgs.tagref
      haskellPackages.haskell-language-server
    ];

    withHoogle = withHoogle;

    shellHook = buildExports;
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv
