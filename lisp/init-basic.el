;;; init-basic.el --- Basic settings. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Basic settings that need no external packages, for better 
;; experience and appearance.
;;
;; Such as:
;;   - Settings that help to improve performance.
;;   - Basic key bindings.
;;   - Basic ui settings that could be applied after startup.
;;

;;; Code:

;; Display the startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Suppress file handlers operations at startup
(unless (or (daemonp) noninteractive init-file-debug)
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Default to utf-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Modify key bindings for macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;; Set mac like common key bindings, like copy/paste/undo/save etc.
(global-set-key (kbd "s-a") 'mark-whole-buffer)   ; select all
(global-set-key (kbd "s-c") 'kill-ring-save)      ; copy
(global-set-key (kbd "s-x") 'kill-region)         ; cut
(global-set-key (kbd "s-s") 'save-buffer)         ; save
(global-set-key (kbd "s-v") 'yank)                ; paste
(global-set-key (kbd "s-z") 'undo)                ; undo

(provide 'init-basic)

;; init-basic.el ends here