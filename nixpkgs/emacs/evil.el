(require 'use-package)

(use-package undo-tree
  :commands global-undo-tree-mode
  :init (global-undo-tree-mode 1))

(use-package evil
  :ensure t
  :init
  ;; for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  ;; other stuff
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

                "fy" 'clip-file
		)

  :config
  ;; makes * and # use emacs-symbols instead of words, otherwise emacs
  ;; only searches for foo in foo_bar
  (setq-default evil-symbol-word-search t)
  ;; make autocompletion case sensitive, otherwise camel case words
  ;; are completed as lowercase
  (setq dabbrev-case-fold-search nil)
  ;; additional maps
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ";") 'evil-repeat-find-char)
    (define-key evil-motion-state-map (kbd ":") 'evil-ex)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; bindings for ediff
(use-package evil-ediff
  :after evil)

(use-package evil-visualstar
  :after evil)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
