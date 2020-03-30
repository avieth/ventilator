{ mkDerivation, base, copilot-c99, copilot-core, copilot-language
, copilot-libraries, copilot-theorem, directory, fetchgit, filepath
, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "copilot";
  version = "3.1";
  src = fetchgit {
    url = "https://github.com/Copilot-Language/Copilot";
    sha256 = "0mwch5yi20ykq0vqsq5aqz6r27bgmfxf6jd2jhbkprj3gyf3pyp0";
    rev = "0f9a1ec66c163f2c99ecd59fa4224328954988a3";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base copilot-c99 copilot-core copilot-language copilot-libraries
    copilot-theorem directory filepath optparse-applicative
  ];
  executableHaskellDepends = [ base copilot-c99 copilot-libraries ];
  homepage = "https://copilot-language.github.io";
  description = "A stream DSL for writing embedded C programs";
  license = stdenv.lib.licenses.bsd3;
}
