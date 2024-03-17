{ mkDerivation, base, containers, fetchgit, hspec, hspec-discover
, lib, QuickCheck, template-haskell
}:
mkDerivation {
  pname = "reflection";
  version = "2.1.7";
  src = fetchgit {
    url = "https://github.com/zoranbosnjak/reflection.git";
    sha256 = "1axlk3aq14kx71prngd6bl56m98y3zkad06d7ac4xbvi2h82z0p3";
    rev = "1724aa3bf600713708d2a7e0ad92560cd53697ed";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base template-haskell ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://github.com/ekmett/reflection";
  description = "Reifies arbitrary terms into types that can be reflected back into terms";
  license = lib.licenses.bsd3;
}
