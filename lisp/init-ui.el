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
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Install nerd-icons for both gui and terminal
;; Use M-x nerd-icons-install-fonts to install Symbols Nerd Fonts Mono
;; nerd-icons: https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons)

;; Color theme
;; doom-themes: https://github.com/doomemacs/themes
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom
  (custom-safe-themes t))

;; Modeline
;; doom-modeline: https://github.com/seagle0128/doom-modeline/tree/master
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-height 25)
  (doom-modeline-vcs-max-length 30)
  (doom-modeline-env-version t)
  (doom-modeline-workspace-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-gruvbox-padded-modeline t))

;; Use minions to make the modeline clean
;; minions: https://github.com/tarsius/minions
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; GUI frame padding
;; spacious-padding: https://protesilaos.com/emacs/
 (use-package spacious-padding
	 :config (spacious-padding-mode))  

(provide 'init-ui)
;; init-ui.el ends here
