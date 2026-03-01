(setq gc-cons-threshold most-positive-fixnum)

(setq byte-compile-warnings '(not obsolete))

(setq warning-suppress-log-types '((comp) (bytecomp)))

(setq native-comp-async-report-warnings-errors 'silent)

(setq inhibit-startup-echo-area-message (user-login-name))

(setq default-frame-alist '((fullscreen . maximized)
    			      (background-color . "#000000")
  			      (ns-appearance . dark)
  			      (ns-transparent-titlebar . t)))

(defvar dysthesis/disable-package-el nil
  "Set non-nil to disable package.el. Preferred override: env JORMUNGANDR_DISABLE_PACKAGE_EL=1.")

(defconst dysthesis/package-el-enabled
  (not (or dysthesis/disable-package-el
           (getenv "JORMUNGANDR_DISABLE_PACKAGE_EL")))
  "True when package.el should be active (default); false in immutable/Nix builds.")

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
  ;; Prefer prebuilt .eln files from the store, but allow runtime native
  ;; compilation into the writable state directory for everything else.
  (when (getenv "JORMUNGANDR_ELN_DIR")
    (require 'comp nil 'noerror)
    (let* ((eln-store (getenv "JORMUNGANDR_ELN_DIR"))
           (eln-state (expand-file-name "eln-cache" user-emacs-directory)))
      ;; Direct new .eln outputs to the writable state cache while still
      ;; keeping the store path first in the load-path.
      (make-directory eln-state t)
      (load "startup" nil t)
      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache eln-state))
      (setq native-comp-eln-load-path (delete-dups (list eln-store eln-state)))
      (setq native-comp-jit-compilation t
            native-comp-deferred-compilation t
            native-comp-async-report-warnings-errors 'silent)))
  ;; Disable network, but still activate Nix-provided packages + autoloads.
  (setq package-enable-at-startup t
        package-archives nil
        package-archive-priorities nil
        package-user-dir package-user-dir
        package-load-list nil
        package--init-file-ensured t)
  (require 'seq)
  (defun dysthesis/nix-elpa-directories ()
    "Return ELPA directories from all Nix profiles."
    (let* ((profiles (split-string (or (getenv "NIX_PROFILES") "") " " t))
           (profile-elpas
            (mapcar (lambda (profile)
                      (expand-file-name "share/emacs/site-lisp/elpa" profile))
                    profiles))
           (load-path-elpas
            (delq nil
                  (mapcar (lambda (p)
                            (when (string-match "emacs-packages-deps.*/share/emacs/site-lisp" p)
                              (expand-file-name "elpa" (file-name-directory (directory-file-name p)))))
                          load-path))))
      (seq-filter #'file-directory-p (delete-dups (append profile-elpas load-path-elpas)))))
  (defun dysthesis/load-profile-autoloads ()
    "Eagerly load autoload files from Nix-provided ELPA directories.
This restores the usual package.el autoload side effects even when
package.el is effectively read-only/immutable."
    (dolist (dir (append (list package-user-dir) (dysthesis/nix-elpa-directories)))
      (when (file-directory-p dir)
        (dolist (pkg (directory-files dir t "^[^.].*"))
          (dolist (autoload (directory-files pkg t "-autoloads\\.el\\'"))
            (when (file-readable-p autoload)
              (load autoload nil 'nomessage)))))))
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
          (delete-dups
           (delq nil
                 (append (list package-user-dir deps-elpa)
                         (dysthesis/nix-elpa-directories))))))
  (make-directory package-user-dir t)
  (dysthesis/load-profile-autoloads)
  (load "package" nil t)
  (package-initialize)
  (setq use-package-always-ensure nil
        use-package-ensure-function (lambda (&rest _args) t)))

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
