{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    simplescreenrecorder
    flameshot
  ];

  home-manager.users.floscr.xdg.configFile = {
    "Dharkael/flameshot.ini".source = <config/flameshot/flameshot.ini>;
  };
}
