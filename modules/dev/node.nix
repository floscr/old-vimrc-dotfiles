{ config, lib, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      nodejs-10_x
      nodePackages.eslint_d
    ];
  };
}
