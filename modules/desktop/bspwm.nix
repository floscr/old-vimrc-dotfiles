{ config, lib, pkgs, ... }:

{
  imports = [ ./. ];

  environment = {
    systemPackages = with pkgs; [
      sxhkd
      bspwm
      rofi
      (polybar.override {
        mpdSupport = true;
        pulseSupport = true;
        nlSupport = true;
      })
      (writeScriptBin "rofi" ''
        #!${stdenv.shell}
        exec ${rofi}/bin/rofi -config "$XDG_CONFIG_HOME/rofi/config" $@
      '')
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
    };

    xdg.configFile = {
      "sxhkd".source = <config/sxhkd>;

        # link recursively so other modules can link files in their folders, e.g.
        # ~/.config/bspwm/rc.d and ~/.config/rofi/theme
        "bspwm" = { source = <config/bspwm>; recursive = true; };
        "rofi" = { source = <config/rofi>; recursive = true; };
    };
  };
}
