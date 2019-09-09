# Virtualbox

{ lib, config, pkgs, ... }:

{
  imports = [
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

  nix.trustedUsers = [ "demo" ];

  services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
  services.xserver.displayManager.sddm.enable = lib.mkForce false;
}
