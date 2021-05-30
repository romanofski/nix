(use-package treemacs
  :general
  (general-nmap :prefix "SPC"
    "0" '(:ignore t :which-key "treemacs")
    "00" 'treemacs))
(use-package treemacs-evil
  :after treemacs evil
  :ensure t)
(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)
(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package projectile
  :general
  (general-nmap :prefix "SPC"
                "p" '(:ignore t :which-key "projectile")
                "p!" 'projectile-run-shell-command-in-root
                "p&" 'projectile-run-async-shell-command-in-root
                "pE" 'projectile-edit-dir-locals
                "pD" 'projectile-dired
                "pF" 'projectile-find-file-in-known-projects
                "pT" 'projectile-test-project
                "pR" 'projectile-regenerate-tags
                "pS" 'projectile-save-project-buffers
                "pt" 'projectile-toggle-between-implementation-and-test
                "pV" 'projectile-browser-dirty-projects
                "pa" 'projectile-find-other-file
                "pb" 'projectile-switch-to-buffer
                "pc" 'projectile-compile-project
                "pd" 'projectile-find-dir
                "pe" 'projectile-recentf
                "pf" 'projectile-find-file
                "pg" 'projectile-find-file-dwim
                "pI" 'projectile-invalidate-cache
                "pj" 'projectile-find-tag
                "pk" 'projectile-kill-buffers
                "pl" 'projectile-find-file-in-directory
                "pm" 'projectile-commander
                "po" 'projectile-multi-occur
                "pp" 'projectile-switch-project
                "pq" 'projectile-switch-open-project
                "pr" 'projectile-replace
                "pu" 'projectile-run-project
                "/"  'projectile-ag)

  :config
  (projectile-mode 1)

  ;; workaround for https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
        '(:eval (format " %s " (projectile-project-name)))))

(use-package helm-projectile
  :defer t
  :init
  (helm-projectile-on)

  :config
  (setq helm-projectile-fuzzy-match t))
