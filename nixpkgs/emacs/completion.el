(use-package company
  :delight
  :config
  (global-company-mode 1)

  (setq company-idle-delay 0.2
        company-selection-wrap-around t)

  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package company-statistics
  :after company
  :init
(company-statistics-mode))