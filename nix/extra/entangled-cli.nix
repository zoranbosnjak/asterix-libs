{ nixpkgs ? import <nixpkgs> {} }:
let
    inherit (nixpkgs) pkgs;

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
        version = "2.4.2";
        src = pkgs.python3Packages.fetchPypi {
          inherit pname;
          inherit version;
          sha256 = "sha256-0UfZt9jqJKyaFAeBnhKuajd/ozaL3JVc8IEmN4wMVr0";
        };
        nativeBuildInputs = [ pkgs.python3Packages.pythonRelaxDepsHook ];
        pythonRelaxDeps = [
            "tomlkit"
            /*
            "click"
            "msgspec"
            "rich-click"
            "watchfiles"
            */
        ];
        propagatedBuildInputs = deps ++ [
            brei
            repl-session
        ];
        doCheck = false;
    };

in entangled-cli

