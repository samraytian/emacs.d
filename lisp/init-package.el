;;; init-package.el --- Package management -*- lexical-binding: t -*-

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

;; Configure package.el before using it
(setq package-quickstart nil
      package-native-compile t
      package-enable-at-startup nil
      package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Configure use-package
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose nil)
  (if (daemonp)
	  (setq use-package-always-demand t)))

(eval-when-compile
  (require 'use-package))

;; Use no-littering to keep .emacs.d clean
;; link: https://github.com/emacscollective/no-littering
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror 'nomessage)
  (setq custom-theme-directory (no-littering-expand-etc-file-name "themes/")))

(provide 'init-package)

;; init-package.el ends here