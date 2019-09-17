{ config, lib, pkgs, ... }:

let nixos-hardware = builtins.fetchTarball https://github.com/NixOS/nixos-hardware/archive/master.tar.gz;
in {
  imports = [
    ./.  # import common settings

    # Hardware specific
    "${nixos-hardware}/lenovo/thinkpad/t490"

    ./modules/desktop/bspwm.nix

    ./modules/browser/firefox.nix
    ./modules/editors/emacs.nix
    ./modules/dev
    ./modules/shell/direnv.nix
    ./modules/shell/git.nix
    ./modules/shell/gnupg.nix
    ./modules/shell/zsh.nix

    # Themes
    ./themes/glimpse
  ];

  boot.initrd.luks.devices = [{
	  name = "root";
	  device = "/dev/nvme0n1p2";
	  preLVM = true;
  }];

  networking.hostName = "thinknix";
  networking.wireless.enable = true;

  # Optimize power use
  environment.systemPackages = [ pkgs.powertop ];
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;

  # Monitor backlight control
  programs.light.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  #
  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.autorun = true;

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
}
