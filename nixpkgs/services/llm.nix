{ secrets, pkgs, nixpkgsUnstable, ... }:

{

  services.ollama = {
    enable = true;
    acceleration = "rocm";
    package =  nixpkgsUnstable.ollama;
  };
}
