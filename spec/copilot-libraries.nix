{ mkDerivation, array, base, containers, copilot-language
, data-reify, fetchgit, mtl, parsec, stdenv
}:
mkDerivation {
  pname = "copilot-libraries";
  version = "3.1";
  src = fetchgit {
    url = "https://github.com/Copilot-Language/Copilot-libraries";
    sha256 = "1a5jq1j9lj511liyjzniq75icm3a7cv2fmgxacy5i8kfivrqjr3d";
    rev = "979cd46cf17b6381826f95a052ce19d4c8c14525";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base containers copilot-language data-reify mtl parsec
  ];
  homepage = "https://copilot-language.github.io";
  description = "Libraries for the Copilot language";
  license = stdenv.lib.licenses.bsd3;
}
