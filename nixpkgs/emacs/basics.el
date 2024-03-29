;;; basics -- generic packages

;;; Commentary:
;;; packages which are so common I want to use them all the time

;;; Code:

(defvar smartparens)
(use-package smartparens
  :delight
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1))

;; which-key helps me remember random keybindings and rediscover things I had
;; lost.
(defvar which-key)
(use-package which-key
  :delight
  :init
  (which-key-mode 1))

;; backup files somewhere outside of where project file watchers will pick them
;; up
;;
;; Put backup files neatly away
;; Kudos to https://github.com/jorgenschaefer/Config/blob/master/emacs.el
(let ((backup-dir "~/.cache/emacs/backups")
      (auto-saves-dir "~/.cache/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        tramp-backup-directory-alist `((".*" . ,backup-dir))
	tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t  ; Don't delink hardlinks
      version-control t  ; Use version numbers on backups
      delete-old-versions t  ; Clean up the backups
      kept-new-versions 20
      kept-old-versions 5)

(defvar flycheck)
(use-package flycheck
  :defer 1
  :init
  (global-flycheck-mode)

  :general
  (general-nmap :prefix "SPC"
	       "e" '(:ignore t :which-key "errors")
	       "el" 'flycheck-list-errors
	       "en" 'next-error
	       "ep" 'previous-error))

(use-package flyspell
  :defer 1
  :custom
  (flyspell-mode 1))

(use-package flyspell-correct-helm
  :after flyspell
  :commands (flyspell-correct-helm)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm)
  :general
  (general-nmap :prefix "SPC"
    "z" '(:ignore t :which-key "spelling")
    "z=" 'flyspell-correct-wrapper))

(use-package highlight-parentheses
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (setq hl-paren-delay 0.2))

;; copy filepath to clipboard
(defun clip-file ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (x-select-text filename)
      (message "Filename copied to clipboard"))))

;; Highlight uncommitted changes using VC
;; Source: https://github.com/dgutov/diff-hl
(use-package diff-hl
  :defer 3
  :config
  (global-diff-hl-mode)
  )

(use-package dired
  :general
  (general-nmap :prefix "SPC"
    "d" '(:ignore t :which-key "dired")
    "dd" 'dired-jump))


;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; avoid processes which watch files trip up on the lock files
;; see https://github.com/angular/angular-cli/issues/18342
(setq create-lockfiles nil)
