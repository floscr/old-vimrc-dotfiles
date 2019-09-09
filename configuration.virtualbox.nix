# Virtualbox

{ lib, config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/virtualbox-demo.nix>
    ../.  # import common settings
    ./modules/desktop/bspwm.nix

    ./modules/dev
    ./modules/editors/emacs.nix
    ./modules/editors/vim.nix
    ./modules/shell/direnv.nix
    ./modules/shell/git.nix
    ./modules/shell/gnupg.nix
    ./modules/shell/zsh.nix

    ./modules/shell/zsh.nix
  ];

  services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
  services.xserver.displayManager.sddm.enable = lib.mkForce false;
}
