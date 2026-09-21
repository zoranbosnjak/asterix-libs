{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs {}
}:
let
    pythonDeps = with pkgs.python3Packages; [
        setuptools
        hatchling
        poetry-core
        argh
        rich
        click
        copier
        filelock
        msgspec
        rich-argparse
        rich-click
        tomlkit
        typeguard
        watchfiles
        pexpect
    ];

    deps = with pkgs; [ python3 ] ++ pythonDeps;

    brei = pkgs.python3Packages.buildPythonPackage rec {
        pname = "brei";
        format = "pyproject";
        version = "0.2.4";
        src = pkgs.python3Packages.fetchPypi {
          inherit pname;
          inherit version;
          sha256 = "sha256-+4ofGRoccKganfNmxTDgGTi4CC5xmOiY1OqOtTBm+pI";
        };
        nativeBuildInputs = [ pkgs.python3Packages.pythonRelaxDepsHook ];
        pythonRelaxDeps = [
            "argh"
            "rich"
        ];
        propagatedBuildInputs = deps;
        doCheck = false;
    };

    repl-session = pkgs.python3Packages.buildPythonPackage rec {
        pname = "repl_session";
        format = "pyproject";
        version = "0.2.0";
        src = pkgs.python3Packages.fetchPypi {
          inherit pname;
          inherit version;
          sha256 = "sha256-R4Bh1+UKaR8EoAKul3bERGgLeBo+N9QDqcVvF3+gH6E";
        };
        propagatedBuildInputs = deps;
        doCheck = false;
    };

    entangled-cli = pkgs.python3Packages.buildPythonPackage rec {
        pname = "entangled_cli";
        format = "pyproject";
        version = "2.4.3";
        src = pkgs.python3Packages.fetchPypi {
          inherit pname;
          inherit version;
          sha256 = "1d3b713b7d53833ff24958bed1b2878c00ed02c4428aa512f8b2ada9bbbf7106";
        };
        nativeBuildInputs = [ pkgs.python3Packages.pythonRelaxDepsHook ];
        pythonRelaxDeps = [
            "tomlkit"
        ];
        propagatedBuildInputs = deps ++ [
            brei
            repl-session
        ];
        doCheck = false;
    };

in entangled-cli

