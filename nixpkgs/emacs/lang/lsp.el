(use-package lsp-mode
  :commands (lsp-deferred)
  :hook ((typescript-mode . lsp-deferred)
         (haskell-mode . lsp-deferred)
         )
  :config
  (setq-default lsp-prefer-flymake t
                lsp-auto-guess-root t
                lsp-enable-snippet t
                lsp-enable-completion-at-point t
                lsp-file-watch-threshold 9000
                lsp-idle-delay 0.500))

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
  :hook ((lsp-mode . lsp-ui-mode))
  )
