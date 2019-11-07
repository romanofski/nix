(use-package magit
  :general
  (general-nmap :prefix "SPC"
                "g" '(:ignore t :which-key "git")
                "gs" 'magit-status
                "gb" 'magit-blame)

  :config
  (setq git-commit-fill-column 72))

(use-package git-timemachine
  :commands git-timemachine-mode
  )
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package evil-magit
  :after magit
  :config
  (general-define-key
   :keymaps '(git-rebase-mode-map)
   :states '(normal)
   "x" 'git-rebase-kill-line
   "s-j" 'git-rebase-move-line-down
   "s-k" 'git-rebase-move-line-up))
