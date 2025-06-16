(use-package powershell)

(use-package copilot-chat
  :general
  (general-nmap :prefix "SPC"
                "c" '(:ignore t :wk "copilot")
                "cd" 'copilot-chat-display
                "cy" 'copilot-chat-yank
                "yp" 'copilot-chat-yank-pop
                "bd" 'evil-delete-buffer
		)
  :config
  ;;(setopt copilot-chat-default-model "claude-sonnet-4")
  (setopt copilot-chat-model "claude-sonnet-4")
  (setopt copilot-chat-frontend 'markdown)
  )

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
