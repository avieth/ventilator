{ mkDerivation, ansi-terminal, base, bimap, containers
, copilot-core, data-default, directory, fetchgit, mtl, parsec
, pretty, process, random, stdenv, transformers, xml
}:
mkDerivation {
  pname = "copilot-theorem";
  version = "3.1";
  src = fetchgit {
    url = "https://github.com/Copilot-Language/Copilot-theorem";
    sha256 = "0lphshz0nf5kmikyf9snymhmjqziniqgxr5vqrm3arlakj6cddzc";
    rev = "4a69decc81bfb522c1ca75d895e8394ea5262d04";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    ansi-terminal base bimap containers copilot-core data-default
    directory mtl parsec pretty process random transformers xml
  ];
  homepage = "https://copilot-language.github.io";
  description = "k-induction for Copilot";
  license = stdenv.lib.licenses.bsd3;
}
