;;; init.el --- Emacs Configuration. -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:

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

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Requisites
(with-temp-message ""
  (require 'init-basic)
  (require 'init-elpa)
  (require 'init-ui)
  )
