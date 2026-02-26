(setq gc-cons-threshold most-positive-fixnum)

(setq byte-compile-warnings '(not obsolete))

(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-echo-area-message (user-login-name))

(defvar dysthesis/disable-package-el nil
  "When non-nil, skip package.el setup (used for Nix builds).")

(setq dysthesis/disable-package-el
      (or dysthesis/disable-package-el
          (getenv "JORMUNGANDR_DISABLE_PACKAGE_EL")))

(let* ((store-dir (file-name-directory (or load-file-name buffer-file-name)))
       (state-root (or (getenv "JORMUNGANDR_STATE_DIR")
                       (expand-file-name "jormungandr"
                                         (or (getenv "XDG_STATE_HOME")
                                             (expand-file-name "~/.local/state/"))))))
  ;; Keep config in the store; direct all writable state elsewhere.
  (setq user-init-file        (expand-file-name "init.el" store-dir)
        early-init-file       (expand-file-name "early-init.el" store-dir)
        user-emacs-directory  (file-name-as-directory
                               (expand-file-name "emacs" state-root))
        package-user-dir      (expand-file-name "elpa" user-emacs-directory)
        package-quickstart    nil
        package-quickstart-file nil))

(when dysthesis/disable-package-el
  ;; Prevent use-package from invoking package.el installers.
  (require 'seq)
  (let* ((deps (seq-find (lambda (p) (string-match "emacs-packages-deps" p))
                         load-path))
         (elpa (when deps (expand-file-name "elpa" deps))))
    (when elpa
      (setq package-user-dir package-user-dir)))
  ;; Keep native-comp outputs in the Nix store and avoid runtime writes.
  (when (getenv "JORMUNGANDR_ELN_DIR")
    (require 'comp nil 'noerror)
    (require 'startup nil 'noerror)
    (let ((eln (getenv "JORMUNGANDR_ELN_DIR")))
      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache eln)
        ;; Keep only store eln path.
        (setq native-comp-eln-load-path (list eln))))
    (setq native-comp-jit-compilation nil
          native-comp-deferred-compilation nil
          native-comp-async-report-warnings-errors 'silent))
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
