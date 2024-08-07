{ stdenv, lib, ... }:

stdenv.mkDerivation rec {
  pname = "geppetto";
  version = "0.1.0";

  src = ./.;

  meta = with lib; {
    description = "G-code manipulation library for Clojure";
    homepage = "https://github.com/eraserhd/geppetto";
    license = licenses.publicDomain;
    platforms = platforms.all;
    maintainers = [ maintainers.eraserhd ];
  };
}
