{ secrets, pkgs, nixpkgsUnstable, ... }:

{
  programs.claude-code = {
    enable = true;
    package = nixpkgsUnstable.claude-code;
    mcpServers = {
      azure-devops = {
        type = "stdio";
        command = "npx";
        args = ["-y" "@azure-devops/mcp@latest" "onedrive"];
      };
    };
  };
  services.ollama = {
    enable = true;
    acceleration = "rocm";
    package =  nixpkgsUnstable.ollama;
  };
}
