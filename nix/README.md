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
nix-prefetch-git [--rev some-rev] https://github.com/zoranbosnjak/asterix-specs.git > nix/aspecs.json
```
