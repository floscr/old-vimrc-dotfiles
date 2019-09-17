{ config, lib, pkgs, ... }:

{
  imports = [ ./. ];

  environment = {
    systemPackages = with pkgs; [
      lightdm
      bspwm
    ];
  };

  fonts.fonts = [ pkgs.siji ];

  services = {
    xserver = {
      desktopManager.xterm.enable = false;
      displayManager.lightdm.enable = true;
      windowManager.bspwm.enable = true;
    };

    compton = {
      enable = true;
      backend = "glx";
      vSync = "opengl-swc";
      inactiveOpacity = "0.90";
    };
  };
}
