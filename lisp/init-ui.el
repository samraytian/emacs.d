;;; init-ui.el --- UI stuff settings.	-*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;

;;; Code:

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

;; Line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Install Icons
(use-package all-the-icons)

;; Color theme
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(provide 'init-ui)

;; init-ui.el ends here
