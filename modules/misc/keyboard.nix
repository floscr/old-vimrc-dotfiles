{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    xorg.xmodmap
  ];

  services.xserver = {
    autoRepeatDelay = 190;
    autoRepeatInterval = 30;
    layout = "us";
  };

  # Restore keyboard modifications that get lost during hibernate
  powerManagement.resumeCommands = ''
    sh ~/bin/setup-keyboard
  '';
}
