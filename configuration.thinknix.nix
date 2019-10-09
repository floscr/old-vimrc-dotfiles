{ config, lib, pkgs, ... }:

let nixos-hardware = builtins.fetchTarball https://github.com/NixOS/nixos-hardware/archive/master.tar.gz;

in {
  imports = [
    ./.  # import common settings

    # Hardware specific
    "${nixos-hardware}/common/cpu/intel"
    "${nixos-hardware}/common/pc/laptop/acpi_call.nix"
    "${nixos-hardware}/common/pc/laptop/cpu-throttling-bug.nix"

    # Desktoop Ui
    ./modules/desktop/bspwm.nix
    ./modules/browser/chromium.nix

    # Dev
    ./modules/editors/emacs.nix
    ./modules/dev/node.nix

    # Shell
    ./modules/shell/gnupg.nix
    ./modules/shell/termite.nix
    ./modules/shell/git.nix
    ./modules/shell/zsh.nix

    # Services
    ./modules/services/syncthing.nix
    ./modules/services/greenclip.nix

    ./modules/misc/keyboard.nix

    # Themes
    ./themes/glimpse
  ];

  nixpkgs.overlays = [
    (import ./overlays/chromium.nix)
  ];

  virtualisation = {
    docker = {
      enable = true;
      autoPrune.enable = true;
      enableOnBoot = false;
      # listenOptions = [];
    };
  };
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
  environment.systemPackages = [ pkgs.powertop pkgs.docker pkgs.docker-compose  ];
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;

  services.undervolt.enable = true;
  services.undervolt.coreOffset= "-110";
  services.undervolt.temp= "95";

  # Monitor backlight control
  programs.light.enable = true;
}
