let
  pkgs = import <nixpkgs> {};
  hpkgs = pkgs.haskellPackages;
in
  hpkgs.developPackage {
    root = ./.;
    modifier = drv: pkgs.haskell.lib.addBuildTools drv [ hpkgs.cabal-install ];
  }
