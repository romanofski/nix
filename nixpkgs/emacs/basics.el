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
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(defvar flycheck)
(use-package flycheck
  :defer 1
  :init
  (global-flycheck-mode)

  :general
  (general-map :prefix "SPC"
	       "e" '(:ignore t :which-key "errors")
	       "el" 'flycheck-list-errors
	       "en" 'next-error
	       "ep" 'previous-error))

;;; basics.el ends here
