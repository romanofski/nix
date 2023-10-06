;; turn off scroll bars (it's in modeline)
(scroll-bar-mode -1)

;; turn off the tool bar (which-key works fine and I've been vimming
;; for long enough that I never use it.)
(tool-bar-mode -1)

(use-package all-the-icons
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;; themes!
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (doom-themes-visual-bell-config)

  (doom-themes-org-config)
  (doom-themes-treemacs-config)

  ;; the 't' argument here tells emacs not to confirm the load is safe
  (load-theme 'doom-solarized-light t)

  ;; TODO: theme switcher
  )

(use-package solaire-mode
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
)

;; pretty modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
  :hook (after-init . doom-modeline-init))

(use-package airline-themes
  :after powerline
  :config
  (setq airline-helm-colors                   nil
        powerline-utf-8-separator-left        #xe0b0
        powerline-utf-8-separator-right       #xe0b2
        airline-utf-glyph-separator-left      #xe0b0
        airline-utf-glyph-separator-right     #xe0b2
        airline-utf-glyph-subseparator-left   #xe0b1
        airline-utf-glyph-subseparator-right  #xe0b3
        airline-utf-glyph-branch              #xe0a0
        airline-utf-glyph-readonly            #xe0a2
        airline-utf-glyph-linenumber          #xe0a1)

  (load-theme 'airline-doom-one t))

;; fonts and ligatures
(when (window-system)
  ;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono NL Medium"))
  ;;(set-face-attribute 'default nil :weight 'normal :height 90 :font "JetBrains Mono NL Medium"))
  (set-frame-font "JetBrains Mono Medium 9" nil t))

;; transparent title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; highlight todos in code.  This package should support:
;;
;; - HOLD
;; - TODO
;; - NEXT
;; - THEM
;; - PROG
;; - OKAY
;; - DONT
;; - FAIL
;; - DONE
;; - NOTE
;; - KLUDGE
;; - HACK
;; - FIXME
;; - XXX
;; - XXXX
;; - ???
(use-package hl-todo
  :defer nil
  :config
  (global-hl-todo-mode))

;; ligature support
(use-package ligature
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package nyan-mode
  :ensure t
  :init (nyan-mode t)
  )

(use-package tree-sitter
    :ensure t

    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
    :ensure t
    :after tree-sitter)
