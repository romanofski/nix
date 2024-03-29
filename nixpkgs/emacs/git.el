(use-package magit
  :general
  (general-nmap :prefix "SPC"
                "g" '(:ignore t :which-key "git")
                "gs" 'magit-status
                "gb" 'magit-blame)
  (:keymaps 'git-rebase-mode-map
            :states 'normal
            "K" #'git-rebase-move-line-up
            "J" #'git-rebase-move-line-down)
  :config
  (setq git-commit-fill-column 72)
  (setq magit-list-refs-sortby "-committerdate"))

(use-package git-timemachine
  :commands git-timemachine-mode
  )
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package auth-source
  :init
  (setq auth-source-debug t))

(use-package auth-source-pass
  :demand t
  :after auth-source
  :init
  (progn
    (setq auth-sources '(password-store))
    )
  )
