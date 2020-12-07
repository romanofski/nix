(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server")
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp)
  )
