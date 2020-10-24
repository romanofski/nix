(use-package lsp-mode
  :commands (lsp-deferred)
  :hook ((typescript-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :config
  (setq-default lsp-prefer-flymake nil
                lsp-auto-guess-root t)
  (bind-key* "M-." #'lsp-goto-type-definition lsp-mode-map))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :init
  (setq-default lsp-ui-flycheck-enable t)
  :hook ((lsp-mode . flycheck-mode)
         (lsp-mode . lsp-ui-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)

  (bind-key "M-U" #'lsp-ui-peek-find-references lsp-mode-map))

(provide 'use-lsp)
