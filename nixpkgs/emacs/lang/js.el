(use-package js2-mode
  :mode "\\.\\(jsx\\|js\\)\\'"
  )

(use-package typescript-mode
  :mode "\\.\\(tsx\\|ts\\)\\'"
  )

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook (
	 (typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 )
  )

(use-package prettier-js
  :after (:any js2-mode tide)
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'fira-code-mode)
  (add-hook 'tide-mode-hook 'prettier-js-mode)
  (add-hook 'tide-mode-hook 'fira-code-mode)
  )

(use-package web-mode
  :ensure t
  :mode (("\\.ts[x]?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode))
  )
