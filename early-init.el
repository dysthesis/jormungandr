;;; early-init.el -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda() (setq gc-cons-threshold (* 50 1024 1024))))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-message t
      inhibit-startup-screen t)

(setq native-comp-speed 2)

;;; early-init.el ends here
