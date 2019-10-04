{ config, lib, pkgs, ... }:

{
  environment = {
    sessionVariables = {
      BROWSER = "chromium";
    };

    systemPackages = with pkgs; [
      chromium
      (pkgs.writeScriptBin "chromium-private" ''
        #! ${pkgs.bash}/bin/bash
        chromium-browser --incognito "$@"
      '')
    ];
  };

  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel ];

  programs.chromium = {
    enable = true;
    extensions = [
      "aomjjhallfgjeglblehebfpbcfeobpgk" # 1Password X
      "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
    ];
  };
}
