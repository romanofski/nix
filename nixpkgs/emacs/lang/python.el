(add-hook 'python-mode-hook #'abbrev-mode)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))
