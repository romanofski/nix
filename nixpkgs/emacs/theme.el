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
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  (solaire-mode-swap-bg))

;; pretty modeline
(use-package doom-modeline
  :ensure t
  :defer t
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
  (set-face-attribute 'default nil :height 100 :family "Fira Code"))

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

;; Fira Code Ligature support
(setq x-meta-keysym 'super x-super-keysym 'meta)
(defun fira-code-mode--make-alist (list)
"Generate prettify-symbols alist from LIST."
(let ((idx -1))
(mapcar
 (lambda (s)
   (setq idx (1+ idx))
   (let* ((code (+ #Xe100 idx))
      (width (string-width s))
      (prefix ())
      (suffix '(?\s (Br . Br)))
      (n 1))
 (while (< n width)
   (setq prefix (append prefix '(?\s (Br . Bl))))
   (setq n (1+ n)))
 (cons s (append prefix suffix (list (decode-char 'ucs code))))))
 list)))

(defconst fira-code-mode--ligatures
'("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
  "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
  "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
  "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
  ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
  "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
  "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
  "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
  ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
  "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
  "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
  "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
  "x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)

(defun fira-code-mode--enable ()
"Enable Fira Code ligatures in current buffer."
(setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
(setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
(prettify-symbols-mode t))

(defun fira-code-mode--disable ()
"Disable Fira Code ligatures in current buffer."
(setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
(prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
"Fira Code ligatures minor mode"
:lighter " Fira Code"
(setq-local prettify-symbols-unprettify-at-point 'right-edge)
(if fira-code-mode
    (fira-code-mode--enable)
  (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
"Setup Fira Code Symbols"
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'fira-code-mode)
;; end Fira Code Ligature support

(use-package nyan-mode
  :ensure t
  :init (nyan-mode t)
  )
