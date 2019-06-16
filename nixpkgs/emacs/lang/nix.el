(use-package nix-mode
  :mode "\\.nix\\'"
  :init
  (add-hook 'after-save-hook 'nix-mode-format)
  )
