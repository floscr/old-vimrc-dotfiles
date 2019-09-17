{ config, lib, pkgs, ... }:

let nixos-hardware = builtins.fetchTarball https://github.com/NixOS/nixos-hardware/archive/master.tar.gz;
in {
  imports = [
    ./.  # import common settings

    # Hardware specific
    "${nixos-hardware}/lenovo/thinkpad/t490"

    ./modules/desktop/bspwm.nix

    # ./modules/browser/firefox.nix
    # ./modules/editors/emacs.nix
    ./modules/dev
    # ./modules/shell/direnv.nix
    # ./modules/shell/git.nix
    # ./modules/shell/gnupg.nix
    # ./modules/shell/zsh.nix

    # Themes
    # ./themes/glimpse
  ];

  # Encrypted Disk
  boot.initrd.luks.devices = [{
	  name = "root";
	  device = "/dev/nvme0n1p2";
	  preLVM = true;
  }];

  # Networking
  networking.hostName = "thinknix";
  # networking.wireless.enable = true;

  # Optimize power use
  environment.systemPackages = [ pkgs.powertop ];
  services.tlp.enable = true;
  powerManagement.powertop.enable = true;

  # Monitor backlight control
  programs.light.enable = true;
}
