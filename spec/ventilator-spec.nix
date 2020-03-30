{ mkDerivation, base, copilot, copilot-c99, stdenv }:
mkDerivation {
  pname = "ventilator-spec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base copilot copilot-c99 ];
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
