{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
, aspecsRef ? builtins.fromJSON (builtins.readFile ../nix/aspecs.json)
}:

let
  aspecsDir = pkgs.fetchgit {
    url = aspecsRef.url;
    rev = aspecsRef.rev;
    sha256 = aspecsRef.sha256;
  };

  aspecs = import "${aspecsDir}/aspecs" {packages=pkgs; inShell=false;};

  deps = [
    aspecs
  ];

  src = builtins.filterSource
    (path: type:
          (type != "directory" || baseNameOf path != ".git")
       && (type != "symlink" || baseNameOf path != "result"))
    ./.;

  drv = pkgs.runCommand "test-specs"
    { propagatedBuildInputs = deps;
    }
    ''
      mkdir $out
      for i in $(ls ${src}/*.ast); do
        echo "test spec: $(basename $i)"
        ${aspecs}/bin/aspecs validate --input-ast $i
        cp $i $out
      done
    '';

  env = pkgs.stdenv.mkDerivation rec {
    name = "devel-environment";
    buildInputs = deps;
    shellHook = ''
      export PATH=${aspecs}/bin:$PATH
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv
