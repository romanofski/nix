(add-hook 'python-mode-hook #'abbrev-mode)
(use-package lsp-python-ms
  :defer 1
  :init
  (setq lsp-python-ms-auto-install-server t
        lsp-python-ms-executable (executable-find "python-language-server"))
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred
