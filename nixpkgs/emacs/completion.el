(use-package company
  :delight
  :config
  (global-company-mode 1)

  (setq company-idle-delay 0.2
        company-selection-wrap-around t)

  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-previous)
  (define-key company-active-map (kbd "C-p") 'company-select-next))

(use-package company-statistics
  :after company
  :init
(company-statistics-mode))

(use-package yasnippet
  :ensure t

  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)

  :config
  (yas-reload-all))
