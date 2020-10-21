
{ pkgs, ... }:

let
  pkgSrc = builtins.fetchTarball {
    # current nixos-unstable HEAD as of 15/08/2020
    url = "https://github.com/NixOS/nixpkgs/archive/32b46dd897ab2143a609988a04d87452f0bbef59.tar.gz";
    sha256 = "1gzfrpjnr1bz9zljsyg3a4zrhk8r927sz761mrgcg56dwinkhpjk";
  };
  pkgsUnstable = import pkgSrc {};
  configs = [
    {
      file = ./emacs/init.el;
      pkgs = epkgs: with epkgs; [
        general
        delight
        use-package
      ];
    }
    {
      file = ./emacs/basics.el;
      pkgs = epkgs: with epkgs; [
        smartparens
        highlight-parentheses
        which-key
        flycheck
        flyspell-correct-helm
      ];
    }
    {
      file = ./emacs/evil.el;
      pkgs = epkgs: with epkgs; [
        evil
        evil-surround
        evil-ediff
        evil-magit
        evil-visualstar
      ];
    }
    {
      file = ./emacs/completion.el;
      pkgs = epkgs: with epkgs; [
        company
        company-statistics
      ];
    }
    {
      file = ./emacs/helm.el;
      pkgs = epkgs: with epkgs; [
        helm
        helm-ag
      ];
    }
    {
      file = ./emacs/theme.el;
      pkgs = epkgs: with epkgs; [
        airline-themes
        doom-themes
        doom-modeline
        solaire-mode
        hl-todo
        nyan-mode
      ];
    }
    {
      file = ./emacs/projects.el;
      pkgs = epkgs: with epkgs; [
        all-the-icons
        helm-projectile
        projectile
        treemacs
        treemacs-evil
        treemacs-icons-dired
        treemacs-projectile
      ];
    }
    {
      file = ./emacs/git.el;
      pkgs = epkgs: with epkgs; [
        magit
        forge
        ghub
        git-timemachine
      ];
    }

    # languages
    {
      file = ./emacs/lang/adoc.el;
      pkgs = epkgs: with epkgs; [
        adoc-mode
        markup-faces
      ];
    }
    {
      file = ./emacs/lang/nix.el;
      pkgs = epkgs: with epkgs; [
        nix-mode
      ];
    }
    {
      file = ./emacs/lang/haskell.el;
      pkgs = epkgs: with epkgs; [
        haskell-mode
        hlint-refactor
        dante
      ];
    }
    {
      file = ./emacs/lang/latex.el;
      pkgs = epkgs: with epkgs; [
        auctex
        company-auctex
      ];
    }
    {
      file = ./emacs/lang/js.el;
      pkgs = epkgs: with epkgs; [
        prettier-js
        web-mode
        typescript-mode
      ];
    }
    {
      file = ./emacs/lang/lsp.el;
      pkgs = epkgs: with epkgs; [
        lsp-mode
        lsp-ui
        lsp-treemacs
        company-lsp
        helm-lsp
      ];
    }
    {
      file = ./emacs/lang/yaml.el;
      pkgs = epkgs: with epkgs; [
        ansible
        yaml-mode
        yasnippet
        auto-complete
      ];
    }
    {
      file = ./emacs/lang/python.el;
      pkgs = epkgs: with epkgs; [
        lsp-python-ms
      ];
    }
  ];
in
{
  programs.emacs.enable = true;
  programs.emacs.extraPackages = epkgs: builtins.concatMap (config: config.pkgs epkgs) configs ++ [];

  home.file.".emacs.d/init.el" = {
    text = builtins.foldl' (acc: config: acc + builtins.readFile config.file + "\n") "" configs;
  };
  home.file.".emacs.d/abbrev_defs" = {
    text = builtins.readFile ./emacs/abbrev_defs.el;
  };

  home.packages = [
    pkgs.emacs-all-the-icons-fonts
    pkgs.nixfmt
    pkgs.aspellDicts.de
    pkgs.aspellDicts.en
    pkgs.python36Packages.jedi
    pkgs.python36Packages.epc
    pkgs.shellcheck
    pkgs.nodePackages.prettier
    pkgs.silver-searcher
    pkgs.fira-code-symbols
    # emacs lsp support
    pkgs.nodePackages.typescript-language-server
    pkgs.nodePackages.typescript
    pkgs.nodePackages.vscode-html-languageserver-bin
    pkgsUnstable.python-language-server
  ];
}
