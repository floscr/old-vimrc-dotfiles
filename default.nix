{ config, lib, pkgs, ...}: with lib;
  {
    imports =
      [
        ./hardware-configuration.nix
      ];

    # Nothing in /tmp should survive a reboot
    boot.cleanTmpDir = true;

    boot.initrd.luks.devices = [{
      name = "root";
      device = "/dev/nvme0n1p2";
      preLVM = true;
    }];

    # Cpu throttling
    services.thermald.enable = true;

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      bc
      coreutils
      firefox
      git
      htop
      killall
      networkmanager
      networkmanagerapplet
      rofi
      unzip
      vim
      wget
    ];

    # Enable sound.
    sound.enable = true;
    hardware.pulseaudio.enable = true;

    # Enable the X11 windowing system.
    services.xserver.enable = true;
    services.xserver.layout = "us";
    # services.xserver.xkbOptions = "eurosign:e";
    services.xserver.windowManager.i3.enable = true;
    services.xserver.autorun = true;

    # Enable touchpad support.
    services.xserver.libinput.enable = true;

    # Enable the KDE Desktop Environment.
    services.xserver.displayManager.sddm.enable = true;
    services.xserver.desktopManager.plasma5.enable = true;

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.floscr = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "video" "networkmanager" ]; # Enable ‘sudo’ for the user.
    };

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "19.03"; # Did you read the comment?
  }
