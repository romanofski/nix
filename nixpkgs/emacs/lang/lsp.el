(use-package powershell)

(use-package eglot
  :hook ((typescript-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (powershell-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         )
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                                                   :plugins (
                                                             :ruff (:enabled t
                                                                    :lineLength 105)
                                                             :yapf (:enabled :json-false)
                                                                   ))))))
