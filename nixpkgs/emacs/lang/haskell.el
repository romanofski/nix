(use-package haskell-mode
  :mode "\\.\\(hs\\|lhs\\|hsc\\|cpphs\\|c2hs\\)\\'"
  :general
  (general-nvmap :keymaps 'haskell-mode-map
                 "gI" 'haskell-navigate-imports)

  (general-nmap :keymaps 'haskell-mode-map
                :prefix ","
                "e" '(:ignore t :which-key "edit")
                "ei" 'haskell-mode-format-imports))

(use-package hindent
  :after haskell-mode
  :general
  (general-nvmap :keymaps 'haskell-mode-map
                 :prefix ","
                 "f" '(:ignore t :which-key "format")
                 "ff" 'hindent-reformat-buffer
                 "fd" 'hindent-reformat-decl)

  :config
  ;; TODO: this isn't always activating. Why?
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (setq hindent-reformat-buffer-on-save t))

(use-package hlint-refactor
  :after haskell-mode
  :general
  (general-nvmap :keymap 'haskell-mode-map
                 :prefix ","
                 "er" 'hlint-refactor-refactor-at-point
		 "eR" 'hlint-refactor-refactor-buffer))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (setq dante-repl-command-line '("cabal" "new-repl"))

  ;; disable checks on idle after change to avoid surprises
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; save the buffer after some idle time to run dante
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 1)
  )

;; also invoke hlint after dante checks
(add-hook 'dante-mode-hook
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint))))
