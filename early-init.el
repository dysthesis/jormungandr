(setq gc-cons-threshold most-positive-fixnum)

(setq byte-compile-warnings '(not obsolete))

(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-echo-area-message (user-login-name))

(defvar dysthesis/disable-package-el nil
  "When non-nil, skip package.el setup (used for Nix builds).")

(setq dysthesis/disable-package-el
      (or dysthesis/disable-package-el
          (getenv "DYSTHESIS_DISABLE_PACKAGE_EL")))

(when dysthesis/disable-package-el
  ;; In Nix build we still want a writable user dir for caches, but keep
  ;; config read-only from the store.
  (let* ((store-dir (file-name-directory (or load-file-name buffer-file-name)))
         (xdg-config (or (getenv "XDG_CONFIG_HOME")
                         (expand-file-name "~/.config/")))
         (nix-user-dir (file-name-as-directory
                        (expand-file-name "emacs-nix" xdg-config))))
    (setq user-emacs-directory nix-user-dir
          ;; Keep loading init from the store copy.
          user-init-file (expand-file-name "init.el" store-dir)
          early-init-file (expand-file-name "early-init.el" store-dir)
          package-user-dir (expand-file-name "elpa" user-emacs-directory)))
  ;; Prevent use-package from invoking package.el installers.
  (require 'seq)
  (let* ((deps (seq-find (lambda (p) (string-match "emacs-packages-deps" p))
                         load-path))
         (elpa (when deps (expand-file-name "elpa" deps))))
    (when elpa
      (setq package-user-dir elpa)))
  (setq package-enable-at-startup t
        package-archives nil
        package-archive-priorities nil)
  (require 'package)
  (package-initialize)
  (setq use-package-always-ensure nil
        use-package-ensure-function
        (lambda (&rest _args) t)))

(setq default-frame-alist '((fullscreen . maximized)
    			      (background-color . "#000000")
  			      (ns-appearance . dark)
  			      (ns-transparent-titlebar . t)))

(when dysthesis/disable-package-el
  (setq package-enable-at-startup nil
        package-quickstart nil
        package-archives nil
        package-archive-priorities nil))

(unless dysthesis/disable-package-el
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

          package-gnupghome-dir (concat package-user-dir "gnupg"))
  (unless (file-exists-p package-user-dir)
    (mkdir package-user-dir t))
  (setopt package-quickstart-file (expand-file-name "package-quickstart.el" "var/cache/"))

  (package-initialize)
  (when (package-installed-p 'compat)
    (require 'compat nil t)))
