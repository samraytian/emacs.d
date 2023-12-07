;;; init-comp.el --- Completion system. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Auto completion system for Emacs using vertico.
;; 

;;; Code:

;; Vertico
;; A better minibuffer interactive completion system
;; link: https://github.com/minad/vertico
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 20)
  (setq vertico-resize t)
  (setq enable-recursive-minibuffers t))

(provide 'init-comp)
;; init-comp.el ends here