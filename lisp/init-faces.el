;;; init-faces.el --- Faces configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;

;;; Code:

;; Fontaine
;; https://protesilaos.com/emacs/fontaine
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

(provide 'init-faces)
;; init-faces.el ends here