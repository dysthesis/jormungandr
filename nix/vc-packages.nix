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
  ghostelZigDeps = pkgs.zig_0_15.fetchDeps {
    pname = "ghostel";
    version = ghostelVersion;
    src = ghostelSrc;
    fetchAll = true;
    hash = "sha256-ghN/UMACgkFQQEr4nH5gbbJbt/+2bz6tL2bJpbw9mGE=";
  };

  ghostel = p.melpaBuild {
    pname = "ghostel";
    version = ghostelVersion;
    commit = vcSources.ghostel.revision;
    src = ghostelSrc;

    nativeBuildInputs = [pkgs.zig_0_15];

    preBuild = ''
      export ZIG_GLOBAL_CACHE_DIR="$TMPDIR/zig-cache"
      mkdir -p "$ZIG_GLOBAL_CACHE_DIR"
      cp -R ${ghostelZigDeps} "$ZIG_GLOBAL_CACHE_DIR/p"
      chmod -R u+w "$ZIG_GLOBAL_CACHE_DIR/p"
      TERM=dumb zig build -Doptimize=ReleaseSafe -Dcpu=baseline
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
