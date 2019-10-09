{ pkgs, ... }:

let
  pkg = pkgs.haskellPackages.greenclip;
in {
  environment.systemPackages = [ pkg pkgs.xorg.libXdmcp ];
  # xdg.configFile."greenclip.cfg".source = ./gc.cfg;

  systemd.services.greenclip = {
      enable      = true;
      description = "greenclip daemon";
      wantedBy = [ "graphical-session.target" ];
      after    = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = ''${pkg}/bin/greenclip daemon'';
      };
  };

  home-manager.users.floscr.xdg.configFile = {
    "greenclip/cfg".source = <config/greenclip/gc.cfg>;
  };
}
