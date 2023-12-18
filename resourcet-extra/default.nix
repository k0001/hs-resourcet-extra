{ mkDerivation, async, base, containers, lib, resourcet
, safe-exceptions, unliftio-core
}:
mkDerivation {
  pname = "resourcet-extra";
  version = "0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    async base containers resourcet safe-exceptions unliftio-core
  ];
  homepage = "https://github.com/k0001/hs-resourcet-extra";
  description = "ResourceT extras";
  license = lib.licenses.bsd3;
}
