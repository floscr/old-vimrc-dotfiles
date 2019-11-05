{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    redshift
    xclip
    xdotool
    ffmpeg
    mpv
    feh
    simplescreenrecorder
    flameshot

    # Useful apps
    # evince    # pdf reader
  ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services = {
    xserver = {
      enable = true;
      libinput.disableWhileTyping = true;
      libinput.enable = true;
    };

    redshift = {
      enable = true;
      latitude = "55.88";
      longitude = "12.5";
      temperature = {
        day = 5500;
        night = 3000;
      };
    };
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;

    fonts = with pkgs; [
      ubuntu_font_family
      dejavu_fonts
      fira-code
      fira-code-symbols
      symbola
      noto-fonts
      noto-fonts-cjk
      font-awesome-ttf
    ];

    fontconfig.defaultFonts = {
      sansSerif = ["Ubuntu"];
      monospace = ["Fira Code"];
    };
  };
}
