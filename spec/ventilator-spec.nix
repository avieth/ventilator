{ mkDerivation, base, copilot, copilot-c99, stdenv }:
mkDerivation {
  pname = "ventilator-spec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base copilot copilot-c99 ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
