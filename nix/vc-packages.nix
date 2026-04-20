{
  pkgs,
  p,
}: let
  inherit
    (pkgs.lib)
    licenses
    mapAttrsToList
    removeAttrs
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

  toSpec = pname: spec:
    {
      inherit pname;
    }
    // spec
    // {
      deps = spec.deps or [];
      version = vcSources."${pname}".version 
		or vcSources."${pname}".revision;
      src = pinSrc vcSources."${pname}";
    };

  # Map packages to dependencies
  packages = {
    eglot-booster = {};
    majutsu = {
      deps = [
        p.magit
        p.magit-section
      ];
      patches = [./patches/majutsu-compile-time.patch];
    };
    nael = {};
    lackluster-theme = {};
  };
in
  mapAttrsToList
  (key: val: (toSpec key val |> mk))
  packages
