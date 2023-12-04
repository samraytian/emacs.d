;;; init.el --- Emacs Configuration. -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samray.tian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:

;;; Code:

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;
;;; Requisites
;;;

(require 'init-basic)
