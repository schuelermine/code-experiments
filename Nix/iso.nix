{ pkgs, config, lib, ... }:
let home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in {
    imports = [
        "${home-manager}/nixos"
        <nixos/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix>
    ];
    home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.nixos = {
            programs = {
                kitty = {
                    enable = true;
                    font = {
                        package = pkgs.jetbrains-mono;
                        name = "JetBrains Mono";
                        size = 13;
                    };
                    keybindings = {
                        "ctrl+tab" = "next_tab";
                        "ctrl+shift+tab" = "previous_tab";
                    };
                    settings = {
                        linux_display_server = "x11";
                    };
                };
                fish = {
                    enable = true;
                    functions = {
                        "..." = ''
                            set -q argv[1] || set argv[1] 2
                            set d "$PWD"
                            for i in (seq $argv[1])
                                set d (dirname $d)
                            end
                            isatty stdout && cd $d || echo $d
                        '';
                    };
                    shellAliases = {
                        ls = "ls -F";
                    };
                    shellInit = ''
                        set -U fish_color_autosuggestion brblack
                        set -U fish_color_cancel --reverse white
                        set -U fish_color_command bryellow
                        set -U fish_color_comment brgreen
                        set -U fish_color_cwd brgreen
                        set -U fish_color_cwd_root brred
                        set -U fish_color_end brmagenta
                        set -U fish_color_error white
                        set -U fish_color_escape yellow
                        set -U fish_color_hg_added green
                        set -U fish_color_hg_clean green
                        set -U fish_color_hg_copied magenta
                        set -U fish_color_hg_deleted red
                        set -U fish_color_hg_dirty red
                        set -U fish_color_hg_modified yellow
                        set -U fish_color_hg_renamed magenta
                        set -U fish_color_hg_unmerged red
                        set -U fish_color_hg_untracked yellow
                        set -U fish_color_history_current --underline
                        set -U fish_color_host normal
                        set -U fish_color_host_remote bryellow
                        set -U fish_color_keyword brcyan
                        set -U fish_color_match --reverse
                        set -U fish_color_normal white
                        set -U fish_color_operator brcyan
                        set -U fish_color_param brblue
                        set -U fish_color_quote brgreen
                        set -U fish_color_redirection brmagenta
                        set -U fish_color_search_match --reverse
                        set -U fish_color_selection --reverse white
                        set -U fish_color_status red
                        set -U fish_color_user brgreen
                        set -U fish_color_valid_path --underline
                        set -U fish_pager_color_background black
                        set -U fish_pager_color_completion normal
                        set -U fish_pager_color_description normal
                        set -U fish_pager_color_prefix normal
                        set -U fish_pager_color_progress --reverse white
                    '';
                };
            };
            gtk = {
                enable = true;
                font = {
                    package = pkgs.ibm-plex;
                    name = "IBM Plex Sans";
                };
                iconTheme = {
                    package = pkgs.yaru-theme;
                    name = "Yaru";
                };
                theme = {
                    package = pkgs.yaru-theme;
                    name = "Yaru-dark";
                };
            };
            qt.style = {
                package = pkgs.libsForQt5.qtstyleplugins;
                name = "gtk2";
            };
            dconf.settings = {
                "org/gnome/shell".enabledExtensions = [
                    "appindicatorsupport@rgcjonas.gmail.com"
                    "user-theme@gnome-shell-extensions.gcampax.github.com"
                ];
                "org/gnome/shell/extensions/user-theme".name = "Yaru";
                "org/gnome/desktop/interface".monospace-font-name = "JetBrains Mono 10";
            };
        };
    };
    environment = {
        gnome.excludePackages = with pkgs.gnome; [
            epiphany
            gnome-calculator
            gnome-terminal
            totem
        ];
        systemPackages = with pkgs; [
            gnome.gnome-tweaks
            gnomeExtensions.appindicator
            kitty
            qalculate-gtk
            firefox
            vlc
            gnome.gnome-sound-recorder
            gimp
            libreoffice
            nano
            wget
            curl
            git
            lsof
            killall
            xorg.xwininfo
            xorg.xkill
            ffmpeg
            file
            libqalculate
            musescore
            inkscape
            audacity
            vscodium
            kdenlive
            blender
            bind.dnsutils
            whois
            sqlite
            sl
            figlet
            toilet
            lolcat
            cowsay
            nnn
            links2
            unicode-paracode
            bat
            xsel
        ];
    };
    services = {
        printing.enable = true;
        gnome.core-developer-tools.enable = true;
        xserver = {
            layout = "de";
            xkbOptions = "eurosign:e";
            xkbVariant = "nodeadkeys";
            libinput.enable = true;
        };
    };
    programs.fish.enable = true;
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_latest;
    networking.hostName = "nixos-live";
    time.timeZone = "Europe/Berlin";
    i18n.defaultLocale = "en_GB.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "de";
    };
    fonts.fonts = with pkgs; [
        ibm-plex
        noto-fonts
        jetbrains-mono
    ];
    users.users.nixos.shell = pkgs.fish;
}
