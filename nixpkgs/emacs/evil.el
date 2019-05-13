(require 'use-package)

(use-package evil
  :init
  (setq evil-want-C-u-scroll 1)
  (evil-mode 1)

  :general
  (general-nmap :prefix "SPC"
                "b" '(:ignore t :wk "buffers")
                "bn" 'evil-next-buffer
                "bp" 'evil-prev-buffer
                "bd" 'evil-delete-buffer

                "w" '(:ignore t :which-key "windows")
                "wh" 'evil-window-left
                "wj" 'evil-window-down
                "wk" 'evil-window-up
                "wl" 'evil-window-right
                "w-" 'evil-window-split
                "TAB" 'evil-switch-to-windows-last-buffer
		)

  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-repeat-find-char)
    (define-key evil-motion-state-map (kbd ":") 'evil-ex)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; bindings for ediff
(use-package evil-ediff
  :after evil)