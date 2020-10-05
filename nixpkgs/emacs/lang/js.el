(use-package web-mode
  :ensure t
  :mode (("\\.ts[x]?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
  )

(use-package typescript-mode
  :mode "\\.ts[x]?\\'")

(use-package prettier-js
  :after (typescript-mode)
  :init
  (add-hook 'web-mode 'prettier-js-mode)
  (add-hook 'lsp-mode-hook 'fira-code-mode)
  )
