{ nixpkgs ? (import <nixpkgs> {}), simpleirc ? (import /home/user/git/SimpleIRC/default.nix {}) }:

let
  inherit (nixpkgs.haskellngPackages) bytestring;
  inherit (nixpkgs.haskellPackages) cabal randomSource;

in
  cabal.mkDerivation (self: {
    pname = "whclan-bot";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    buildDepends = [ simpleirc bytestring randomSource ];
    meta = {
      homepage = "whclan.uk.to";
      description = "irc bot for .. um .. our clan. uh, why are you reading this?";
      license = self.stdenv.lib.licenses.publicDomain;
      platforms = self.ghc.meta.platforms;
    };
  })
