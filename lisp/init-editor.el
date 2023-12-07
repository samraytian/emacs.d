;;; init-editor.el --- Editor behavior. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:
;;
;; Default editor behavior and settings.
;;

;;; Code:

;; Directly modify selected text
(delete-selection-mode t)

;; Auto reload file if it is changed by external program
(setq auto-revert-verbose nil
      auto-revert-interval 10
      auto-revert-remote-files t
      auto-revert-check-vc-info t
      global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

(provide 'init-editor)
;; init-editor.el ends here