;;; init.el --- Emacs Configuration. -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samraytian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:

;;; Code:

;; Fix the issue of the title bar becomes taller on macOS 14 while using emacs-mac
;; This is a workaround, and should be removed when the bug is fixed.
(when (featurep 'mac)
  (add-hook 'window-setup-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0))))