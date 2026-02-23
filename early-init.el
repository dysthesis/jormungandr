(setq gc-cons-threshold most-positive-fixnum)

(setq byte-compile-warnings '(not obsolete))

(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-echo-area-message (user-login-name))

(setq default-frame-alist '((fullscreen . maximized)
    			      (background-color . "#000000")
  			      (ns-appearance . dark)
  			      (ns-transparent-titlebar . t)))

(require 'package)
(setopt package-archives
        '(("elpa" . "https://elpa.gnu.org/packages/")
          ("elpa-devel" . "https://elpa.gnu.org/devel/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/"))

        package-archive-priorities
        '(;; Prefer development packages
          ("elpa-devel" . 99)
          ("melpa" . 90))

        package-user-dir (expand-file-name "elpa/" "var/")
        package-gnupghome-dir (concat package-user-dir "gnupg"))
(unless (file-exists-p package-user-dir)
  (mkdir package-user-dir t))
(setopt package-quickstart-file (expand-file-name "package-quickstart.el" "var/cache/"))

(package-initialize)
(when (package-installed-p 'compat)
  (require 'compat nil t))
