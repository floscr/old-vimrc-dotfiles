{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    xmodmap
  ];

  services.xserver = {
    autoRepeatDelay = 300;
    autoRepeatInterval = 45;
    layout = "us";
  };
}
