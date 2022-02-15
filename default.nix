{ pkgs
}:
let
  src = pkgs.lib.sourceByRegex ./. [
    ".*.hs"
    ".*.cabal"
    "cabal.project"
    "README.md"
    "CHANGELOG.md"
    "LICENSE"
  ];
  haskPkgs = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: {
      xmobar = haskellPackagesOld.xmobar.overrideAttrs (old: {
        configureFlags = "-f all_extensions";
      });
    };
  };
  drv = (haskPkgs.callCabal2nix "xmonad-config" src { });
in
drv
