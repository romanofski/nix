
{ pkgs, ... }:

let
  sources = import ./nixpkgsource.nix;
  pkgsUnstable = import sources.nixos-unstable { };
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
        diff-hl
      ];
    }
    {
      file = ./emacs/evil.el;
      pkgs = epkgs: with epkgs; [
        evil
        evil-surround
        evil-collection
        evil-visualstar
        undo-tree
      ];
    }
    {
      file = ./emacs/completion.el;
      pkgs = epkgs: with epkgs; [
        company
        company-statistics
        yasnippet
      ];
    }
    {
      file = ./emacs/helm.el;
      pkgs = epkgs: with epkgs; [
        helm
        helm-rg
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
        all-the-icons-dired
        tree-sitter
        tree-sitter-langs
      ];
    }
    {
      file = ./emacs/projects.el;
      pkgs = epkgs: with epkgs; [
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
      file = ./emacs/lang/lsp.el;
      pkgs = epkgs: with epkgs; [
        helm-lsp
        bazel
        powershell
      ];
    }
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
        lsp-haskell
        flycheck-haskell
      ];
    }
    {
      file = ./emacs/lang/js.el;
      pkgs = epkgs: with epkgs; [
        prettier-js
        web-mode
        typescript-mode
        ecukes
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
        lsp-pyright
        yapfify
        pyvenv
      ];
    }
    {
      file = ./emacs/lang/terraform.el;
      pkgs = epkgs: with epkgs; [
        terraform-mode
      ];
    }
  ];
in
{
  programs.emacs = {
    enable = true;
    package = pkgsUnstable.emacs29-gtk3;
    extraPackages = epkgs: builtins.concatMap (config: config.pkgs epkgs) configs ++ [];
  };

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
    pkgs.shellcheck
    pkgs.nodePackages.prettier
    pkgs.ripgrep
    pkgs.jetbrains-mono
    pkgs.discount
    # emacs lsp support
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.hie-bios
    pkgs.nodePackages.typescript-language-server
    pkgs.nodePackages.typescript
    pkgs.nodePackages.vscode-html-languageserver-bin
    pkgs.nodePackages.eslint
    pkgs.nodePackages.yaml-language-server
    pkgs.pyright
    pkgs.python39Packages.isort
    pkgsUnstable.python39Packages.pylint
    pkgs.python39Packages.yapf
  ];
}
