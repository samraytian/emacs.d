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
  :ensure t
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
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

;; Support pinyin first letter matching for Chinese characters in orderless, avy etc
;; link: https://github.com/cute-jumper/pinyinlib.el
(use-package pinyinlib
  :ensure t
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; Marginalia
;; Rich annotations in the minibuffer
;; link: https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Consult
;; Useful search and navigation commands
;; link: https://github.com/minad/consult

;; Embark
;; Minibuffer actions and context menu
;; link: https://github.com/oantolin/embark

;; Corfu
;; A completion framework that offers the same features as Ivy or Helm, but with a much smaller footprint
;; link: https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)    ; configure space for separator insertion
        ("M-q" . corfu-quick-complete)      ; use C-g to exit
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous)
        ([escape] . corfu-quit))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  :config
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq corfu-popupinfo-delay 0.2))

;; Cape
;; A collection of useful completion at point extensions
;; link: https://github.com/minad/cape
(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)     ; programming language keyword
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)

  :config
  (setq cape-dict-file (expand-file-name "etc/hunspell_dict.txt" user-emacs-directory))

  ;; for Eshell:
  ;; ===========
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  )

(provide 'init-comp)
;; init-comp.el ends here
