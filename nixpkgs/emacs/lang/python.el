(add-hook 'python-mode-hook #'abbrev-mode)
(use-package py-isort)

(use-package company-jedi
  :ensure t)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
