{ mkDerivation, base, copilot, copilot-c99, stdenv }:
mkDerivation {
  pname = "ventilator-spec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base copilot copilot-c99 ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
