{ sources ? import ../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, inShell ? null
}:

let

  deps = [
    pkgs.python3Packages.mypy
    pkgs.python3Packages.pytest
    pkgs.python3Packages.hypothesis
    pkgs.python3Packages.autopep8
    pkgs.python3Packages.build
    pkgs.python3Packages.setuptools
    pkgs.python3Packages.twine
  ];

  customPython = pkgs.python3.buildEnv.override {
    extraLibs = deps;
  };

  code-generator = import ../../code-generator/default.nix { inShell = false; };

  aspecsRef = builtins.fromJSON (builtins.readFile ../../nix/aspecs.json);
    aspecsDir = pkgs.fetchgit {
    url = aspecsRef.url;
    rev = aspecsRef.rev;
    sha256 = aspecsRef.sha256;
  };

  testSpecs = import ../../test-specs/default.nix { inShell = false; };

  drv = pkgs.python3Packages.buildPythonPackage rec {
    name = "libasterix";
    src = ./.;
    pyproject = true;
    propagatedBuildInputs = deps;
    nativeCheckInputs = [
      pkgs.python3Packages.pytest
    ];
    checkPhase = ''
      runHook preCheck
      pytest
      runHook postCheck
    '';
    postInstall = ''
      currDir=$(pwd)
      cd $out/lib/python*/site-packages/asterix
      touch py.typed
      cd $currDir
    '';
  };

  env = pkgs.stdenv.mkDerivation rec {
    name = "python-environment";
    buildInputs = [
      customPython
      code-generator
    ];

    shellHook = ''
      export PYTHONPATH=$(pwd)/src:$PYTHONPATH
      export PATH=${code-generator}/bin:$PATH
      export ASTERIX_SPECS_REF="git:${aspecsRef.rev}"
      export ASTERIX_SPECS_DATE="${aspecsRef.date}"
      export ASTERIX_SPECS_FILES=$(find ${aspecsDir}/specs/cat* | grep "\.ast")
      export TEST_ASTERIX_SPECS_FILES=$(find ${testSpecs}/* | grep "\.ast")
    '';
  };

in if inShell == false
   then drv
   else if pkgs.lib.inNixShell then env else drv
