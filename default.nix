let
  nixpkgs_unstable = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-unstable";
  pkgs_unstable = import nixpkgs_unstable { config = {}; overlays = []; };
in

pkgs_unstable.mkShell {
  buildInputs = [
    pkgs_unstable.zig
  ];
}

