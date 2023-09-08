(use-package powershell)

(use-package eglot
  :hook ((typescript-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (powershell-mode . eglot-ensure)
         ))
