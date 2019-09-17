{ config, lib, pkgs, ...}: with lib;
  {
    # Hostname
    networking.hostName = "thinknix";

    nixpkgs.config.allowUnfree = true;

    # Nothing in /tmp should survive a reboot
    boot.cleanTmpDir = true;

    # Bootloader
	  boot.loader.systemd-boot.enable = true;
	  boot.loader.efi.canTouchEfiVariables = true;

    # Cpu throttling
    services.thermald.enable = true;

    environment = {
      systemPackages = with pkgs; [
        # Just the bear necessities~
        coreutils
        git
        wget
        vim
        htop
        networkmanager
        networkmanagerapplet
        killall
        wget
        unzip
        bc
        # Support for extra filesystems
        bashmount  # convenient mounting
        sshfs
        exfat
        ntfs3g
        hfsprogs
      ];
      variables = {
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_BIN_HOME = "$HOME/.local/bin";
      };
      shellAliases = {
        nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
        ne = "nix-env";
        nu = "sudo nix-channel --update && sudo nixos-rebuild -I config=$HOME/.dotfiles/config switch";
        nre = "sudo nixos-rebuild -I config=$HOME/.dotfiles/config";
        ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
      };
    };

    services.xserver = {
      enable = true;
      layout = "en";
      xkbOptions = "eurosign:e";

      windowManager.bspwm.enable = true;
      windowManager.default = "bspwm";
      windowManager.bspwm.configFile = "/etc/dotfiles/config/bspwm/bspwmrc";
      windowManager.bspwm.sxhkd.configFile= "/etc/dotfiles/config/sxhkd/sxhkdrc";
      desktopManager.xterm.enable = false;

      displayManager.auto = {
        enable = true;
        user = "sky";
      };

      # Graphic
      videoDrivers = ["intel"];

      # Notebook
      synaptics.enable = true;
      synaptics.twoFingerScroll = true;
    };

    #Time
    time.timeZone = "Europe/Vienna";

    # Set up user account
    users.users.floscr = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "video" "networkmanager" ];
    };

    # Hardware
    hardware = {
      bluetooth.enable = true;
      bluetooth.powerOnBoot = false;
    };

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "19.03"; # Did you read the comment?
  }
