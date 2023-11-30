;;; init.el --- Emacs Configuration. -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2023 Samray Tian

;; Author: Samray Tian <samraytian@gmail.com>
;; URL: https://github.com/samraytian/emacs.d

;;; Commentary:

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)