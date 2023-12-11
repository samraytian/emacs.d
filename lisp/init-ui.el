;;; init-ui.el --- UI stuff settings.	-*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;

;;; Code:

;; Set frame settings for deamon mode
(when (daemonp)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-scroll-bar-mode nil)
)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

;; Line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode
	 '(org-mode-hook
	   term-mode-hook
	   help-mode-hook
	   shell-mode-hook
	   treemacs-mode-hook
	   eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Install nerd-icons for both gui and terminal
;; Use M-x nerd-icons-install-fonts to install Symbols Nerd Fonts Mono
;; nerd-icons: https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons)

;; GUI frame padding
;; spacious-padding: https://protesilaos.com/emacs/
(use-package spacious-padding
  :config (spacious-padding-mode))  

;; Modeline
;; doom-modeline: https://github.com/seagle0128/doom-modeline/tree/master
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-height 30)
  (doom-modeline-vcs-max-length 30)
  (doom-modeline-env-version t)
  (doom-modeline-workspace-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project))

;; Use minions to make the modeline clean
;; minions: https://github.com/tarsius/minions
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(provide 'init-ui)
;; init-ui.el ends here
