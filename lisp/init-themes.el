;;; init-themes.el --- Load theme. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;

;;; Code:

;; doom-themes
;; https://github.com/doomemacs/themes
(use-package doom-themes
  :ensure t
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom
  ((doom-gruvbox-dark-variant "hard")
   (doom-theme-enable-bold t)
   (doom-theme-enable-italic t)
   (custom-safe-themes t)))

;; ef themes
;; https://protesilaos.com/emacs/ef-themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-region '(intense no-extend neutral))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui nil)
  )

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load theme
;; (load-theme 'ef-light :no-confirm)
(load-theme 'doom-gruvbox :no-confirm)

(dolist (mode '(prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
		   (variable-pitch-mode 0)
       (kill-local-variable 'line-spacing)
		   (setq-local default-text-properties '(line-spacing 0.18 line-height 1.18)))))

(custom-set-faces
    '(default ((t :family "Monaspace Neon" :height 130)))
    '(bold ((t :weight semibold)))
    '(font-lock-comment-face ((t :italic t))))

(provide 'init-themes)
;; init-themes.el ends here
