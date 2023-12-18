;;; init-basics.el --- Basic settings. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Basic settings for better experience.
;;

;;; Code:

;; Default to utf-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Use shorter answers for `yes-or-no-p'
(setopt use-short-answers t)

;; Keep emacs home dir clean
(use-package no-littering
  :ensure t)

;; Prevent to create lockfiles
(setq create-lockfiles nil)

;; Save history
(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-autosave-interval 300)
  (setq enable-recursive-minibuffers t)
  (setq savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (setq savehist-file (no-littering-expand-var-file-name "history")))

;; Save place, open file at last edit position
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (no-littering-expand-var-file-name "places")))

(provide 'init-basics)
;; init-basics.el ends here
