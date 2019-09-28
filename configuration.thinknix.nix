{ config, lib, pkgs, ... }:

let nixos-hardware = builtins.fetchTarball https://github.com/NixOS/nixos-hardware/archive/master.tar.gz;

in {
  imports = [
    ./.  # import common settings

    # Hardware specific
    "${nixos-hardware}/lenovo/thinkpad/t490"

    # Desktoop Ui
    ./modules/desktop/bspwm.nix
    ./modules/browser/chromium.nix

    # Dev
    ./modules/editors/emacs.nix

    # Shell
    ./modules/shell/gnupg.nix
    ./modules/shell/termite.nix
    ./modules/shell/git.nix
    ./modules/shell/zsh.nix

    # Services
    ./modules/services/syncthing.nix

    ./modules/misc/keyboard.nix

    # Themes
    # ./themes/glimpse
  ];

  # Encrypted Disk
  boot.initrd.luks.devices = [{
    name = "root";
    device = "/dev/nvme0n1p2";
    preLVM = true;
  }];

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  fonts = {
    fontconfig = {
      dpi = 180;
      enable = true;
      antialias = true;
      defaultFonts.monospace = [ "Iosevka" ];
      allowBitmaps = true;
      useEmbeddedBitmaps = true;
      ultimate = {
        enable = true;
        substitutions = "combi";
      };
    };
    fonts = with pkgs; [
      fira-code-symbols
      iosevka
      noto-fonts
      symbola
      noto-fonts-cjk
      font-awesome_5
    ];
  };

  # Networking
  networking.hostName = "thinknix";
  networking.networkmanager.enable = true;

  # Optimize power use
  environment.systemPackages = [ pkgs.powertop ];
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;

  # Monitor backlight control
  programs.light.enable = true;
}
