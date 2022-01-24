with builtins; rec {
  fixFlake =
    flake:
      if
        isFunction flake
      then
        let
          args = mapListToAttrs () (functionArgs flake);
        in
          foo (flake args)
      else
        if
          isAttrs flake
        then
          bar
        else notAFlake;

  notAFlake = abort "Not a flake.";

  mapListToAttrs = f: xs: listToAttrs (map f xs);
  mapAttrsToList = f: as: attrValues (mapAttrs (k: f k as.${k}));
  mapAttrs' = f: as: listToAttrs (k: f k as.${k}) (attrNames as);

  registry = {
    linux-repos = {
      tytau = {
        uglysoup = "uglysoup";
      };
      nopkgs = {
        nonstdenv = "nonstdenv";
      };
      full = {
        ybps = "ypbs";
      };
      inapt = {
        repkg = "repkg";
      };
      anchorage = {
        submerge = "submerge";
      };
      misc = {
        inky = "inky";
      };
    };
    linux-packages = {
      hello = "hello";
      bash = "bash";
      linux = "linux";
      firefox = "firefox";
      systemd = "systemd";
      gnome = "gnome";
      kde = "kde";
      python = "python";
      perl = "perl";
      libreoffice = "libreoffice";
      vlc = "vlc";
      audacity = "audacity";
      ffmpeg = "ffmpeg";
      less = "less";
      procps = "procps";
      coreutils = "coreutils";
      busybox = "busybox";
      git = "git";
      nix = "nix";
      subversion = "subversion";
      mercurial = "mercurial";
      tar = "tar";
      gzip = "gzip";
      m4 = "m4";
      gcc = "gcc";
      as = "as";
      passwd = "passwd";
      xorg = "xorg";
      gimp = "gimp";
      inkscape = "inkscape";
      emacs = "emacs";
      vim = "vim";
      ed = "ed";
      sed = "sed";
      gawk = "gawk";
      epiphany = "epiphany";
      grep = "grep";
      jq = "jq";
      zsh = "zsh";
      powershell = "powershell";
      fish = "fish";
      nodejs = "nodejs";
      rustc = "rustc";
      openjdk = "openjdk";
      cargo = "cargo";
      dotnet = "dotnet";
      cabal = "cabal";
      ghc = "ghc";
      R = "R";
      codeblocks = "codeblocks";
      go = "go";
      swift = "swift";
      sqlite = "sqlite";
      kate = "kate";
      glibc = "glibc";
      musl = "musl";
      gmp = "gmp";
      php = "php";
      kotlin = "kotlin";
      mono = "mono";
      krita = "krita";
      vscode = "vscode";
      diff = "diff";
      julia = "julia";
      scala = "scala";
      gnome-builder = "gnome-builder";
      clisp = "clisp";
      scheme = "scheme";
      dapl = "dapl";
      qt = "qt";
      gtk = "gtk";
      imagemagick = "imagemagick";
      terminfo = "terminfo";
      ncurses = "ncurses";
      bluez = "bluez";
      pipewire = "pipewire";
      wayland = "wayland";
      gedit = "gedit";
      pulseaudio = "pulseaudio";
      jack = "jack";
      man-db = "man-db";
      gio = "gio";
      cryptsetup = "cryptsetup";
      cups = "cups";
      dbus = "dbus";
      udev = "udev";
      iproute = "iproute";
      iptables = "iptables";
      bind = "bind";
      nginx = "nginx";
      caddy = "caddy";
      httpd = "httpd";
      dnsutils = "dnsutils";
      wireshark = "wireshark";
      net-tools = "net-tools";
      alsa = "alsa";
      dmesg = "dmesg";
      eclipse = "eclipse";
      atom = "atom";
      android-studio = "android-studio";
      texinfo = "texinfo";
      pycharm-community = "pycharm-community";
      intellij-idea-community = "intellij-idea-community";
      transmission = "transmission";
      super-tux-kart = "super-tux-kart";
      sudo = "sudo";
      doas = "doas";
      kubernetes = "kubernetes";
      docker = "docker";
      qemu = "qemu";
      wine = "wine";
    };
  };
}