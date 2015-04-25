{ nixpkgs ? (import <nixpkgs> {}), simpleirc ? (import /home/MagneticDuck/git/clones/SimpleIRC/default.nix {}) }:

let
  inherit (nixpkgs.haskellPackages) cabal;
  inherit (nixpkgs.haskellngPackages) bytestring;

in
  cabal.mkDerivation (self: {
    pname = "whclan-bot";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    buildDepends = [ simpleirc bytestring ];
    meta = {
      homepage = "whclan.uk.to";
      description = "irc bot for .. um .. our clan. uh, why are you reading this?";
      license = self.stdenv.lib.licenses.publicDomain;
      platforms = self.ghc.meta.platforms;
    };
  })
