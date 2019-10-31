{ config, pkgs, options, lib, ...}:
{
  imports = [
    <home-manager/nixos>
  ];

  # Nothing in /tmp should survive a reboot
  boot.cleanTmpDir = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];

  nix = {
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" ];
    nixPath = options.nix.nixPath.default ++ [
      "config=/etc/dotfiles/config"
    ];
  };
  nixpkgs.config.allowUnfree = true;

  # Cpu throttling
  services.thermald.enable = true;

  # services.udiskie = {
  #   enable = true;
  #   notify = false;
  # };

  services.fwupd.enable = true;
  # services.thinkfan.enable = true;

  environment = {
    systemPackages = with pkgs; [
      # fan control
      s-tui
      # thinkfan
      fwupd
      udiskie
      undervolt
      bc
      coreutils
      tree
      git
      htop
      killall
      networkmanager
      networkmanagerapplet
      rofi
      unzip
      vim
      wget
      neofetch
      (ripgrep.override { withPCRE2 = true; })
    ];
    variables = {
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_BIN_HOME = "$HOME/.local/bin";
      DOTFILES = "$HOME/.dotfiles";
    };
    shellAliases = {
      q = "exit";
      nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
      ne = "nix-env";
      nu = "sudo nix-channel --update && sudo nixos-rebuild -I config=$HOME/.dotfiles/config switch";
      ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
      nre = "sudo nixos-rebuild switch -I config=/etc/dotfiles/config";
      sudo = "sudo ";
    };
  };

  time.timeZone = "Europe/Vienna";

  # Enable sound.
  sound.enable = true;

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "opengl-swc";
    inactiveOpacity = "0.90";
  };

  services.xserver = {
    enable = true;
    autorun = true;

    videoDrivers = ["intel"];

    desktopManager.xterm.enable = false;
    windowManager.bspwm.enable = true;
    displayManager.lightdm.enable = true;

    # Enable touchpad support.
    libinput.enable = true;

    displayManager.sessionCommands = ''
# disable Display Power Managing Signaling
xset -dpms

greenclip daemon&

sh ~/.config/polybar/launch.sh&
sh ~/bin/setup-keyboard
    '';
  };

  users.users.floscr = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "video" "networkmanager" "docker" ];
    shell = pkgs.zsh;
  };

  home-manager.users.floscr = {
    xdg.enable = true;
    home.file."bin" = {
      source = ./bin;
      recursive = true;
    };
  };

  # programs.home-manager.enable = true;
  # programs.home-manager.path = https://github.com/rycee/home-manager/archive/master.tar.gz;

  # home-manager.users.floscr = {
  #   programs.zsh.enable = true;
  # };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
