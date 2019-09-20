{ config, lib, pkgs, ... }:

{
  imports = [ ./. ];

  environment = {
    systemPackages = with pkgs; [
      bspwm
    ];
  };

  home-manager.users.floscr = {
    programs.rofi = {
      enable = true;
      extraConfig = ''
        rofi.modi: window,run,ssh,combi
        rofi.ssh-client: mosh
        rofi.ssh-command: {terminal} -e "{ssh-client} {host}"
        rofi.combi-modi: window,drun,ssh
        rofi.font: Iosevka 21
      '';
      terminal = "termite";
      theme = "flat-orange";
    };

    xdg.configFile = {
      # "bspwm/bspwmrc".source = <config/bspwm/bspwmrc>;
      "sxhkd/sxhkdrc".source = <config/sxhkd/sxhkdrc>;
      # "rofi" = {
      #   source = <config/rofi>;
      #   recursive = true;
      # };
    };
  };
}
