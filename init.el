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

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Modify key bindings for macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; Set mac like common key bindings, like copy/paste/undo/save etc.
(global-set-key (kbd "s-a") 'mark-whole-buffer)   ; select all
(global-set-key (kbd "s-c") 'kill-ring-save)      ; copy
(global-set-key (kbd "s-x") 'kill-region)         ; cut
(global-set-key (kbd "s-s") 'save-buffer)         ; save
(global-set-key (kbd "s-v") 'yank)                ; paste
(global-set-key (kbd "s-z") 'undo)                ; undo

;; Requisites

(require 'init-ui)