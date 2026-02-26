(setq gc-cons-threshold most-positive-fixnum)

(setq byte-compile-warnings '(not obsolete))

(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-echo-area-message (user-login-name))

(defvar dysthesis/disable-package-el nil
  "Set non-nil to disable package.el. Preferred override: env JORMUNGANDR_DISABLE_PACKAGE_EL=1.")

(defconst dysthesis/package-el-enabled
  (not (or dysthesis/disable-package-el
           (getenv "JORMUNGANDR_DISABLE_PACKAGE_EL")))
  "True when package.el should be active (default); false in immutable/Nix builds.")

;; Compute immutable config location (Nix store) and mutable state location.
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
        package-quickstart-file (expand-file-name "package-quickstart.el" user-emacs-directory)))
(make-directory user-emacs-directory t)

(unless dysthesis/package-el-enabled
  ;; Keep native-comp outputs in the Nix store and avoid runtime writes.
  (when (getenv "JORMUNGANDR_ELN_DIR")
    (require 'comp nil 'noerror)
    (let ((eln (getenv "JORMUNGANDR_ELN_DIR")))
      ;; Try to load startup.el quietly to get startup-redirect-eln-cache.
      (load "startup" nil t)
      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache eln)
        ;; Keep only store eln path.
        (setq native-comp-eln-load-path (list eln))))
    (setq native-comp-jit-compilation nil
          native-comp-deferred-compilation nil
          native-comp-async-report-warnings-errors 'silent))

  ;; Disable network, but still activate Nix-provided packages + autoloads.
  (setq package-enable-at-startup t
        package-archives nil
        package-archive-priorities nil
        package-user-dir package-user-dir
        package-load-list nil
        package--init-file-ensured t)
  (require 'seq)
  (let* ((deps-elpa-entry (seq-find (lambda (p)
                                      (string-match "emacs-packages-deps[^ ]*/share/emacs/site-lisp/elpa" p))
                                    load-path))
         (deps-elpa (cond
                     ((not deps-elpa-entry) nil)
                     ((string-match "/elpa/" deps-elpa-entry)
                      (file-name-directory (directory-file-name deps-elpa-entry)))
                     ((string-suffix-p "/elpa" deps-elpa-entry) deps-elpa-entry)
                     (t deps-elpa-entry))))
    (setq package-directory-list
          (delete-dups (delq nil (list package-user-dir deps-elpa)))))
  (make-directory package-user-dir t)
  (load "package" nil t)
  (package-initialize)
  (setq use-package-always-ensure nil
        use-package-ensure-function (lambda (&rest _args) t)))

(setq default-frame-alist '((fullscreen . maximized)
    			      (background-color . "#000000")
  			      (ns-appearance . dark)
  			      (ns-transparent-titlebar . t)))

(when dysthesis/package-el-enabled
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
