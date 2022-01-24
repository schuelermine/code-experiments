{ specialArgs.symlink = nix-symlink.symlink;
specialArgs = { symlink = nix-symlink.symlink; };
specialArgs = { inherit (nix-symlink) symlink; }; } //


# It takes a whole lot of characters for this to become efficient
rec {
  x.b = 1;

  c = x;
  a1.b = c.b;
  a2 = { b = c.b; };
  a3 = { inherit (c) b; };

  phonex-bisoxen = x;
  g1robix-poshids.bileph-nureech = phonex-bisoxen.bileph-nureech;
  g2robix-poshids = { bileph-nureech = phonex-bisoxen.bileph-nureech; };
  g3robix-poshids = { inherit (phonex-bisoxen) bileph-nureech; };

  moped-supplement-decoy = x;
  a1ppleton-diaphragm-dublin.sublimate-pilfered-ingrate = moped-supplement-decoy.sublimate-pilfered-ingrate;
  a2ppleton-diaphragm-dublin = { sublimate-pilfered-ingrate = moped-supplement-decoy.sublimate-pilfered-ingrate; };
  a3ppleton-diaphragm-dublin = { inherit (moped-supplement-decoy) sublimate-pilfered-ingrate; };
}
