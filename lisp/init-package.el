;;; init-package.el --- Package management.  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Package management configuration.
;;

;;; Code:

(eval-when-compile
  (require 'package))

;; Add package-archives
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")
	      ("gnu"    . "https://elpa.gnu.org/packages/")
	      ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Set package archive priorities
(setq package-archive-priorities
      '(("melpa"  . 20)
        ("org"    . 10)
        ("gnu"    . 5)
        ("nongnu" . 0)))

;; Configure package.el before using it
(setq package-quickstart nil
      package-native-compile t
      package-enable-at-startup nil)

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Configure use-package
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose nil)
  (if (daemonp)
	  (setq use-package-always-demand t)))

(eval-when-compile
  (require 'use-package))

;; Use packages that integrate with use-package
(use-package use-package-ensure-system-package)
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

(provide 'init-package)
;; init-package.el ends here