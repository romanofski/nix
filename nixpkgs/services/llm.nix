{ secrets, pkgs, nixpkgsUnstable, ... }:

{
  home.packages = with pkgs; [
    gh
    nixpkgsUnstable.github-copilot-cli
  ];
  services.ollama = {
    enable = true;
    acceleration = "rocm";
    package =  nixpkgsUnstable.ollama;
  };
}
