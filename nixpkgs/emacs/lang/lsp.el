(use-package lsp-mode
  :commands (lsp-deferred)
  :hook ((typescript-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :config
  (setq-default lsp-prefer-flymake nil
                lsp-auto-guess-root t
                lsp-enable-snippet t
                lsp-idle-delay 0.500))

(use-package company-lsp
  :after company lsp-mode
  :config
  (setq company-lsp-enable-snippet t)
  :init
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-delay 0.7)
  :hook ((lsp-mode . flycheck-mode)
         (lsp-mode . lsp-ui-mode))
  :config
  (setq lsp-ui-flycheck-enable t)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'lsp-ui 'javascript-eslint))

(provide 'use-lsp)
