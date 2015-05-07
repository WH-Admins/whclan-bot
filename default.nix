{ nixpkgs ? (import <nixpkgs> {}) }:

let
  inherit (nixpkgs) fetchgit;

  simpleirc-source =
    fetchgit 
      { url = "git://github.com/MagneticDuck/simpleirc.git";
        sha256 = "8175187abcfa14aaf899f14c01e61b4f3dad425f37ddf6fc097e2f1573f7c071"; };
  simpleirc = import simpleirc-source {};

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
