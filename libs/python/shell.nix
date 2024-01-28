{ sources ? import ../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

let
  customPython = pkgs.python3.buildEnv.override {
    extraLibs = [
      pkgs.python3Packages.mypy
      pkgs.python3Packages.pytest
      pkgs.python3Packages.hypothesis
      pkgs.python3Packages.autopep8
    ];
  };

in pkgs.stdenv.mkDerivation rec {
  name = "python-environment";

  buildInputs = [
    customPython
  ];

  shellHook = ''
    export PYTHONPATH=$(pwd):$PYTHONPATH
  '';
}
