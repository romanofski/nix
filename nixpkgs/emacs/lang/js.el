(use-package js2-mode
  :mode "\\.\\(tsx\\|jsx\\|js\\|ts\\)\\'"
  )

(use-package prettier-js
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  )
