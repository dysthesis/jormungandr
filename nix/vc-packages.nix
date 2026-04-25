{
  pkgs,
  p,
}: let
  inherit
    (pkgs.lib)
    licenses
    mapAttrsToList
    removeAttrs
    removePrefix
    ;

  vcSources = import ./npins {};

  mk = args @ {
    pname,
    version ? "git",
    src,
    meta ? {},
    deps ? [],
    ...
  }:
    p.trivialBuild ((removeAttrs args ["deps" "meta"])
      // {
        inherit pname version src;
        packageRequires = deps;
        meta =
          {
            license = licenses.gpl3Plus;
          }
          // meta;
      });

  pinSrc = pin: pin {inherit pkgs;};

  versionFor = pname:
    vcSources."${pname}".version or vcSources."${pname}".revision;

  srcFor = pname: pinSrc vcSources."${pname}";

  toSpec = pname: spec:
    {
      inherit pname;
    }
    // spec
    // {
      deps = spec.deps or [];
      version = versionFor pname;
      src = srcFor pname;
    };

  ghostelSrc = srcFor "ghostel";
  ghostelRelease = versionFor "ghostel";
  ghostelVersion = removePrefix "v" ghostelRelease;
  ghostelModuleExt = pkgs.stdenv.hostPlatform.extensions.sharedLibrary;
  ghostelSystem = pkgs.stdenv.hostPlatform.system;
  ghostelModulePlatforms = {
    "x86_64-linux" = "x86_64-linux";
    "aarch64-linux" = "aarch64-linux";
    "x86_64-darwin" = "x86_64-macos";
    "aarch64-darwin" = "aarch64-macos";
  };
  ghostelModuleHashes = {
    "x86_64-linux" = "sha256-42L9Y53DnlEUEo4EAyLvdgQdx8rErn9iaTapfADd8Nw=";
    "aarch64-linux" = "sha256-Gf6H5f7RsA16TSKLAp97l/j5meOFCpyZzSFiLvm7scQ=";
    "x86_64-darwin" = "sha256-iSEC1G5D4VybxiaRXlVF5pc++DL2PuID8irF2z4HV6k=";
    "aarch64-darwin" = "sha256-DMsjRip62u35gOhPPCE0crkdcnZB9UIvyJfNq8FyT3s=";
  };
  ghostelModulePlatform =
    ghostelModulePlatforms.${ghostelSystem}
    or (builtins.throw "ghostel has no upstream module asset for ${ghostelSystem}");
  ghostelModuleName = "ghostel-module-${ghostelModulePlatform}${ghostelModuleExt}";
  ghostelModule = pkgs.fetchurl {
    url = "https://github.com/dakra/ghostel/releases/download/${ghostelRelease}/${ghostelModuleName}";
    hash =
      ghostelModuleHashes.${ghostelSystem}
      or (builtins.throw "ghostel has no upstream module hash for ${ghostelSystem}");
  };

  ghostel = p.melpaBuild {
    pname = "ghostel";
    version = ghostelVersion;
    commit = vcSources.ghostel.revision;
    src = ghostelSrc;

    preBuild = ''
      install -m755 ${ghostelModule} "ghostel-module${ghostelModuleExt}"
    '';

    files = ''(:defaults "etc" "ghostel-module${ghostelModuleExt}")'';

    meta = {
      license = licenses.gpl3Plus;
      homepage = "https://github.com/dakra/ghostel";
      description = "Terminal emulator powered by libghostty";
    };
  };

  # Map packages to dependencies
  packages = {
    eglot-booster = {};
    majutsu = {
      deps = with p; [
        magit
        magit-section
      ];
      patches = [./patches/majutsu-compile-time.patch];
    };
    nael = {};
    lackluster-theme = {};
    typst-ts-mode = {};
    "typst-preview.el" = {
      deps = with p; [
        websocket
      ];
    };
  };
in
  mapAttrsToList
  (key: val: (toSpec key val |> mk))
  packages
  ++ [ghostel]
