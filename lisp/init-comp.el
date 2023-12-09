;;; init-comp.el --- Completion system. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Auto completion using vertico, orderless, consult, corfu, etc.
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

;; Orderless
;; A completion style that gives completions in order of relevance
;; link: https://github.com/oantolin/orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

;; Support pinyin first letter matching for Chinese characters in orderless, avy etc
;; link: https://github.com/cute-jumper/pinyinlib.el
(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(provide 'init-comp)
;; init-comp.el ends here