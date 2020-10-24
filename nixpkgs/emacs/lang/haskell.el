(use-package haskell-mode
  :mode "\\.\\(hs\\|lhs\\|hsc\\|cpphs\\|c2hs\\)\\'"
  :init
  (add-hook 'haskell-mode-hook 'fira-code-mode)
  :general
  (general-nvmap :keymaps 'haskell-mode-map
                 "gI" 'haskell-navigate-imports)

  (general-nmap :keymaps 'haskell-mode-map
                :prefix ","
                "e" '(:ignore t :which-key "edit")
                "ei" 'haskell-mode-format-imports)
  )
(use-package lsp-haskell
  :ensure t
  :init
  (setq lsp-haskell-process-path-hie "haskell-language-server")
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp)
  )

(use-package flycheck-haskell
  :ensure t
  )
