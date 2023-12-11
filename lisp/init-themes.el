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
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom
  (custom-safe-themes t))

;; ef themes
;; https://protesilaos.com/emacs/ef-themes
(use-package ef-themes
  :init
  (setq ef-themes-region '(intense no-extend neutral))

  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui nil)
  )

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load theme
(load-theme 'ef-light :no-confirm)

(provide 'init-themes)
;; init-themes.el ends here