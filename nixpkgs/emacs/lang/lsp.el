(use-package powershell)

(use-package treesit
  :init
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (bicep "https://github.com/josteink/bicep-ts-mode")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (css-mode . css-ts-mode)
     (dockerfile-mode . dockerfile-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (tsx-mode . tsx-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode . yaml-ts-mode))))

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
  (setopt copilot-chat-default-model "gpt-5.2")
  (setopt copilot-chat-frontend 'markdown)
  )

(setq bicep-ts-mode-default-langserver-path
      (expand-file-name "~/.nix-profile/bin/Bicep.LangServer"))

;; bicep-ts-mode has an autoload call to eglot, loading
;; bicep-register-langserver. So load it before we load eglot, otherwise
;; eglot initialises and has no clue about bicep-ts-mode
(use-package bicep-ts-mode
 :mode ("\\.bicep\\'" . bicep-ts-mode)
 :demand t ;; Force to load immediately so that bicep-ts-mode functions are available
 :config
 (with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(bicep-ts-mode . (,bicep-ts-mode-default-langserver-path))))
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

(use-package yaml-ts-mode
 :ensure t
 :mode ("\\.ya?ml\\'" . yaml-ts-mode)
 )
