localFlake: rec {
  # localFlake is imported via :lf, can’t do that in this file alone with just getFlake

  flakeC = localFlake.nixosConfigurations.buggeryyacht-nixos;
  nixos = builtins.getFlake "github:NixOS/nixpkgs/nixos-unstable";
  oldC = (nixos.lib.nixosSystem {
    system = builtins.currentSystem;
    modules = [ ~/old-config/configuration.nix ];
  });
  cmp = u.cmpSets (flakeC.config) (oldC.config);

  # I’m trying to compare these configs

  u = with builtins; rec {
    cmpSets = v1: v2:
      if typeOf v1 != "set" || typeOf v2 != "set" then {
        type = "directComparison";
        inherit v1 v2;
        eq = v1 == v2;
        fullyEqual = v1 == v2;
      } else
        let
          f = k: v:
            if v1 ? ${k} && v2 ? ${k} then {
              type = "sharedKey";
              v1 = v1.${k};
              v2 = v2.${k};
              eq = cmpSets v1.${k} v2.${k};
              fullyEqual = v1.${k} == v2.${k};
            } else {
              type = "missing";
              v1 = v1.${k} or null;
              v2 = v2.${k} or null;
              eq = if v1 ? ${k} then 1 else 2;
              fullyEqual = v1.${k} or null == v2.${k} or null;
            };
        in mapAttrs f (v1 // v2);
  };

  filterAttrs = with builtins;
    p: s:
    foldl' (s1: s2: s1 // s2) { }
    (map (k: let v = s.${k}; in if p v then { ${k} = v; } else { })
      (attrNames s));

  b = t:
    filterAttrs (s:
      let attempt = builtins.tryEval (s.fullyEqual or true);
      in if attempt.success then attempt.value else t) cmp;

  # I tried this to compare them, along with just

  n = builtins.attrNames cmp;
  x = map (k: cmp.${k}.fullyEqual) n;
}
