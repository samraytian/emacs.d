;;; early-init.el --- Early initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samraytian@gmail.com>
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

;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; No title bar in Linux's GUI mode
;; using `undecorated` or `undecorated-rounded' for round corners
(when (and (eq system-type 'gnu/linux) (display-graphic-p))
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Natural title bar with no text in macOS's GUI mode
(when (or (featurep 'ns) (featurep 'mac))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

(provide 'early-init)

;;; early-init.el ends here
