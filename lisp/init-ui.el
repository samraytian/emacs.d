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

;; Themes
;; ef themes: https://protesilaos.com/emacs/ef-themes
(use-package ef-themes
  :init
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  (setq ef-themes-region '(intense no-extend neutral))

  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui nil)
  
  ;; Load the theme of choice:
  (load-theme 'ef-light :no-confirm)
  )

;; GUI frame padding
;; spacious-padding: https://protesilaos.com/emacs/
(use-package spacious-padding
  :config (spacious-padding-mode))  

;; Fonts
;; Fontaine: https://protesilaos.com/emacs/fontaine
(use-package fontaine
  :when (display-graphic-p)
  :config
  (setq fontaine-latest-state-file
	(no-littering-expand-etc-file-name "fontaine-latest-state.eld"))

  (setq fontaine-presets
	'((regular
	   :default-height 130
	   :default-weight regular
	   :fixed-pitch-height 1.0
	   :variable-pitch-height 1.0
	   )
	  (large
	   :default-height 180
	   :default-weight normal
	   :fixed-pitch-height 1.0
	   :variable-pitch-height 1.05
	   )
	  (t
	   :default-family "Monaspace Neon"
	   :fixed-pitch-family "Monaspace Neon"
	   :variable-pitch-family "Open Sans"
	   :italic-family "Monaspace Neon"
	   :bold-weight semibold
	   :italic-slant italic)
	  ))
  
  ;; Persist font configurations while switching themes (doing it with
  ;; my `modus-themes' and `ef-themes' via the hooks they provide).
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset))
  )

(fontaine-set-preset 'regular)

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
