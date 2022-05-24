(use-package helm
  :delight
  :general
  (general-nvmap :prefix "SPC"
                 ":" 'helm-M-x

                 "fr" 'helm-mini
                 "bb" 'helm-mini

                 ;; helm meta
                 "hr" 'helm-resume

                 ;; jumping prefix defined in jumping.el
                 "jj" 'helm-semantic-or-imenu

                 "y" '(:ignore t :wk "yanking")
                 "yp" 'helm-show-kill-ring
                 "ff" 'helm-find-files)

  :config
  (helm-mode 1)

  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
      (define-key keymap (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-z") 'helm-select-action)
      )
    )

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-function 'pop-to-buffer
        helm-file-cache-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-window-prefer-horizontal-split t))


(use-package helm-rg
  :after helm)
