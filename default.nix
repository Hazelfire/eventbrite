{ mkDerivation, aeson, base, bytestring, hpack, lens, stdenv, text
, wreq
}:
mkDerivation {
  pname = "eventbrite";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base bytestring lens text wreq ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring lens text wreq
  ];
  testHaskellDepends = [ aeson base bytestring lens text wreq ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/eventbrite#readme";
  license = stdenv.lib.licenses.bsd3;
}
