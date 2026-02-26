{ pkgs }:
let
  emacs = pkgs.emacs-nox;
in
pkgs.stdenv.mkDerivation {
  pname = "jormungandr-tangle";
  version = "0.1";

  src = pkgs.nix-gitignore.gitignoreSource [
    ".git"
    ".direnv"
    "result"
  ] ../.;

  nativeBuildInputs = [ emacs pkgs.coreutils pkgs.findutils ];

  buildPhase = ''
    runHook preBuild
    export HOME="$TMPDIR"
    export LANG=C.UTF-8
    emacs -Q --batch -l org \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(org-babel-tangle-file \"README.org\")"
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp README.org init.el early-init.el "$out/"
    if [ -d themes ]; then
      cp -r themes "$out/"
    fi
    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Tangling step for jormungandr Emacs config";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
  };
}
