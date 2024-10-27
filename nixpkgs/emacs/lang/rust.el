;; for tree sitter
(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :init
  ;; configure for eglot
  (setq rustic-lsp-client 'eglot)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  )
