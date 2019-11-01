{ config, lib, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      nodejs-10_x
      nodePackages.npm
      nodePackages.eslint_d
    ];
  };
}
