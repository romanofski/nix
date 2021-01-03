(use-package web-mode
  :ensure t
  :mode (("\\.ts[x]?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
  )

(use-package typescript-mode
  :mode "\\.ts[x]?\\'"
  :config
  (add-hook 'lsp-after-initialize-hook (lambda
                                         ()
                                         (flycheck-add-next-checker 'lsp 'javascript-eslint)
                                         )))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  )
