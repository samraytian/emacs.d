;;; init.el --- Emacs Configuration. -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:

;;; Code:

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Emacs %s or higher is required!" minver)))

;; Display the startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Emacs custom file settings
(setq custom-file
      (expand-file-name "etc/custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Requisites
(with-temp-message ""
  (require 'init-paclage)   ;; package manager
  (require 'init-basics)    ;; emacs basic settings
  (require 'init-keys)      ;; keybindings
  (require 'init-ui)        ;; theme, font, modeline, etc
  )
