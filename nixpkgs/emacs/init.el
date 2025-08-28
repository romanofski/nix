;;; init.el --- user init file -*- no-byte-compile: t -*-

;;; Commentary:
;;; Bootstrap everything.  As little as possible should live here.

;;; Code:

;; enable package.
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; use-package dependencies
(require 'use-package)

;; we assume these are available as use-package stanzas in other configs
(use-package delight)
(use-package general
  :init
  (general-evil-setup))

;; disable tabs globally
(setq-default indent-tabs-mode nil)

;; show traceback on error
(setq debug-on-error t)
