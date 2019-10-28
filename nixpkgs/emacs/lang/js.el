(use-package js2-mode
  :mode "\\.\\(jsx\\|js\\)\\'"
  )

(use-package typescript-mode
  :mode "\\.\\(tsx\\|ts\\)\\'"
  )

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :init
  (setq tide-tsserver-executable "~/works/slsecure/config-frontend/frontend/node_modules/.bin/tsserver")
  :hook (
	 (typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 )
  )

(use-package prettier-js
  :after (:any js2-mode tide)
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'tide-mode-hook 'prettier-js-mode)
  )
