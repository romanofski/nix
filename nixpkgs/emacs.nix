
{ pkgs, ... }:

let
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
        which-key
        flycheck
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
        evil-magit
        magit
        git-timemachine
      ];
    }

    # languages
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
        hindent
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
        js2-mode
        typescript-mode
        tide
        prettier-js
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
}
