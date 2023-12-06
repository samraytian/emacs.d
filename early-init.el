;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; GC settings
;; Increasing the gc-cons-threshold during startup can speed up the loading time. 
;; Then decrease it back to a more reasonable value after startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 16777216)))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package initialization,
;; so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; No title bar in Linux
;; using `undecorated` or `undecorated-rounded' for round corners
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Natural title bar in macOS
(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format '("%n %b"))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Early frame settings before fisrt frame is created
(setq default-frame-alist
  (append (list
    '(font . "Monaspace Neon-14")
    '(width . 120)
    '(height . 50))
    default-frame-alist))

(setq initial-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars . nil)))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable startup screen
(setq inhibit-startup-screen t)

(provide 'early-init)

;;; early-init.el ends here
