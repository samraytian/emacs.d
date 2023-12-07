;;; init-keys.el --- Key bindings. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;

;;; Code:

;; Modify key bindings for macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; Delete unusful key bindings
(global-unset-key (kbd "C-z"))      ; suspend-frame
(global-unset-key (kbd "C-x C-z"))  ; suspend-frame
(global-unset-key (kbd "s-q"))      ; save-buffers-kill-terminal
(global-unset-key (kbd "M-z"))      ; zap-to-char

;; Set mac like common key bindings, like copy/paste/undo/save etc.
(global-set-key (kbd "s-a") 'mark-whole-buffer)   ; select all
(global-set-key (kbd "s-c") 'kill-ring-save)      ; copy
(global-set-key (kbd "s-x") 'kill-region)         ; cut
(global-set-key (kbd "s-s") 'save-buffer)         ; save
(global-set-key (kbd "s-v") 'yank)                ; paste
(global-set-key (kbd "s-z") 'undo)                ; undo

(provide 'init-keys)
;; init-keys.el ends here