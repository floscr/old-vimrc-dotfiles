#Enables bluetooth headset using bluez5 and pulseaudio
{ pkgs, ... }:

let
  # Spotify is terrible on hidpi screens (retina, 4k); this small wrapper
  # passes a command-line flag to force better scaling.
  spotify-4k = pkgs.symlinkJoin {
    name = "spotify";
    paths = [ pkgs.spotify ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/spotify \
        --add-flags "--force-device-scale-factor=1.5"
    '';
  };

in
{
  #sudo rfkill unblock bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    extraConfig = ''
        ControllerMode = bredr
        [Headset]
        HFP=true
        MaxConnected=1
        FastConnectable=true
        [General]
        Disable=Headset
        AutoEnable=true
        MultiProfile=multiple
        AutoConnect=true
        Enable=Source,Sink,Media,Socket
        '';
  };
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
    extraModules = [ unstable.pulseaudio-modules-bt ];
  };

 # hardware.pulseaudio.configFile = pkgs.runCommand "default.pa" {} ''
 #    pacmd set-card-profile 3 a2dp_sink
 #  '';

  nixpkgs.config = {
    packageOverrides = pkgs: {
      bluez = pkgs.bluez5;
    };
  };

  environment.systemPackages = [
    pkgs.playerctl
    pkgs.pavucontrol
    pkgs.blueman
    spotify-4k
  ];
}
