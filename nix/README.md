# Update procedures

# sources.json

Run commands in project base (outside of `nix/`) directory.

```bash
# update niv
nix-shell -p niv --run "niv init"

# update packages
nix-shell -p niv --run "niv update nixpkgs -b master"
nix-shell -p niv --run "niv update nixpkgs -b release-..."
```

## aspecs

```bash
# nix-env -i nix-prefetch-scripts
nix-prefetch-git https://github.com/zoranbosnjak/asterix-specs.git > nix/aspecs.json
# or
hsh=...
nix-prefetch-git --rev ${hsh} https://github.com/zoranbosnjak/asterix-specs.git \
    > nix/aspecs.json
```

## haskell packages to nix

`cabal2nix` can be used instead of `nix-prefetch-git`, to get a `.nix`
file directly. For example.

```bash
hsh=...
cabal2nix --revision ${hsh} https://github.com/user/proj.git > nix/proj.nix
```
