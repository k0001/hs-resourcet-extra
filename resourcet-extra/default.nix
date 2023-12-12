{ mkDerivation, base, containers, lib, resourcet, safe-exceptions
}:
mkDerivation {
  pname = "resourcet-extra";
  version = "0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base containers resourcet safe-exceptions
  ];
  homepage = "https://github.com/k0001/hs-resourcet-extra";
  description = "ResourceT extras";
  license = lib.licenses.bsd3;
}
