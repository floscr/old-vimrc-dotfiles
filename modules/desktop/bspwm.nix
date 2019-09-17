{ config, lib, pkgs, ... }:

{
  imports = [ ./. ];

  environment = {
    systemPackages = with pkgs; [
      lightdm
      bspwm
      # dunst
      # libnotify

      # (polybar.override {
      #   # i3support = true;
      #   # pulseSupport = true;
      #   # nlSupport = true;
      # })
      # killall

      # rofi
      # (writeScriptBin "rofi" ''
      #   #!${stdenv.shell}
      #   exec ${rofi}/bin/rofi -config "$XDG_CONFIG_HOME/rofi/config" $@
      # '')

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
