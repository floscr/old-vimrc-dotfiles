{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    syncthing
  ];
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "floscr";
    configDir = "/home/floscr/.config/syncthing";
    dataDir = "/home/floscr/sync";
  };
}
