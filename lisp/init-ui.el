;;; init-ui.el --- UI configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Visual (UI) configuration for better Emacs experience and looks.
;;

;;; Code:

;; Initial frame settings
(when (display-graphic-p)
  (setq initial-frame-alist '((top . 0.5) 
                              (left . 0.5) 
                              (width . 0.628) 
                              (height . 0.8))))

(provide 'init-ui)

;;; init-ui.el ends here
