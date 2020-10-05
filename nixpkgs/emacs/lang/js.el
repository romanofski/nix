(use-package web-mode
  :ensure t
  :mode (("\\.ts[x]?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
  )

(use-package typescript-mode
  :mode "\\.ts[x]?\\'")

(use-package prettier-js
  :after (:any typescript-mode)
  :init
  (add-hook 'lsp-mode-hook 'prettier-js-mode)
  (add-hook 'lsp-mode-hook 'fira-code-mode)
  )
