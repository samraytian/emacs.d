;;; init-basic.el --- Basic settings. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Basic settings for better experience and appearance.
;;

;;; Code:

;; Default to utf-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

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

;; Emacs custom file settings
(setq custom-file
      (expand-file-name "etc/custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Emacs auto-save-list path settings
(setq auto-save-list-file-prefix
      (expand-file-name "var/auto-save-list/saves-" user-emacs-directory))

;; Emacs backup directory settings
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "var/backup/" user-emacs-directory))))

;; Emacs lockfile settings
(setq create-lockfiles nil)

(provide 'init-basic)

;; init-basic.el ends here
