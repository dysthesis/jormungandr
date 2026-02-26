{ pkgs, p }:
let
  inherit (pkgs.lib) licenses;
  eglot-booster-src = pkgs.fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "cab7803c4f0adc7fff9da6680f90110674bb7a22";
    sha256 = "11kb8n79p0d9bm3v3c5v0qmn6sqbzhppzxd4f9qrdy3h1anm0h65";
  };
  majutsu-src = pkgs.fetchFromGitHub {
    owner = "0WD0";
    repo = "majutsu";
    rev = "20a62f0c3707a84b9097157ea670541aa2ab696e";
    sha256 = "1b840z3p10jyh8d6kmj7syad7308qr9p09gsci4gmha0iw3adnx5";
  };
  nael-src = pkgs.fetchzip {
    url = "https://codeberg.org/mekeor/nael/archive/v0.8.1.tar.gz";
    sha256 = "0ncw44d34xdwsvincl92v52h69chcwfp9jn0rg1vizhxd3dag7hd";
  };
  mk = { pname, version ? "git", src, meta ? {}, deps ? [] }: p.trivialBuild {
    inherit pname version src;
    packageRequires = deps;
    meta = {
      license = licenses.gpl3Plus;
    } // meta;
  };
in [
  (mk { pname = "eglot-booster"; src = eglot-booster-src; })
  (mk { pname = "majutsu"; version = "0.6.0"; src = majutsu-src; deps = [ p.magit ]; })
  (mk { pname = "nael"; version = "0.8.1"; src = nael-src; })
]
