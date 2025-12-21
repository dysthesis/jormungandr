(defvar emacs--backup-gc-cons-threshold gc-cons-threshold)
(defvar emacs--backup-gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defun emacs--restore-gc-values ()
  "Restore garbage collection values to the original defaults."
  (setq gc-cons-threshold emacs--backup-gc-cons-threshold)
  (setq gc-cons-percentage emacs--backup-gc-cons-percentage))

(if ;; if native compilation is available in the current build...
    (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; ...enable native compilation of packages...
    (setq package-native-compile t)
  ;; ...otherwise delete it from the list of features.
  (setq features (delq 'native-compile features)))

(setq native-comp-warning-on-missing-source nil
      native-comp-async-report-warnings-errors nil
      byte-compile-warnings nil
      byte-compile-verbose nil)

(setq jka-compr-verbose nil)

(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

(setq inhibit-compacting-font-caches t)

(when (and (not (daemonp))
           (not noninteractive))
  (setq frame-resize-pixelwise t)
  (setq frame-inhibit-implied-resize t)
  (setq auto-mode-case-fold nil)
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)
  (setq bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t)
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (advice-add 'display-startup-screen :override #'ignore)
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

(defvar emacs--old-file-name-handler-alist (default-toplevel-value
                                            'file-name-handler-alist))

(defun emacs--respect-file-handlers (fn args-left)
  "Respect file handlers.
FN is the function and ARGS-LEFT is the same argument as `command-line-1'.
Emacs processes command-line files very early in startup. These files may
include special paths like TRAMP paths, so restore `file-name-handler-alist' for
this stage of initialization."
  (let ((file-name-handler-alist (if args-left
                                     emacs--old-file-name-handler-alist
                                   file-name-handler-alist)))
    (funcall fn args-left)))

(defun emacs--restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'."
  (set-default-toplevel-value
   'file-name-handler-alist
   ;; Merge instead of overwrite to preserve any changes made since startup.
   (delete-dups (append file-name-handler-alist
                        emacs--old-file-name-handler-alist))))

(when (and emacs-optimize-file-name-handler-alist
           (not (daemonp))
           (not emacs-debug))
  ;; Determine the state of bundled libraries using calc-loaddefs.el. If
  ;; compressed, retain the gzip handler in `file-name-handler-alist`. If
  ;; compiled or neither, omit the gzip handler during startup for improved
  ;; startup and package load time.
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (locate-file-internal "calc-loaddefs.el" load-path)
       nil
     (list (rassq 'jka-compr-handler
                  emacs--old-file-name-handler-alist))))

  ;; Ensure the new value persists through any current let-binding.
  (put 'file-name-handler-alist 'initial-value
       emacs--old-file-name-handler-alist)

  ;; Emacs processes command-line files very early in startup. These files may
  ;; include special paths TRAMP. Restore `file-name-handler-alist'.
  (advice-add 'command-line-1 :around #'emacs--respect-file-handlers)

  (add-hook 'emacs-startup-hook #'emacs--restore-file-name-handler-alist
            101))

(defun emacs--reset-inhibit-redisplay ()
  "Reset inhibit redisplay."
  (setq-default inhibit-redisplay nil)
  (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay))

(when (and (not (daemonp))
           (not noninteractive)
           (not minimal-emacs-debug))
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t)
  (add-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay -100))

(defun minimal-emacs--reset-inhibit-message ()
  "Reset inhibit message."
  (setq-default inhibit-message nil)
  (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibit-message))

(when (and (not (daemonp))
           (not noninteractive)
           (not minimal-emacs-debug))
  (setq-default inhibit-message t)
  (add-hook 'post-command-hook #'minimal-emacs--reset-inhibit-message -100))

(when (and (not (daemonp))
           (not noninteractive)
           (not minimal-emacs-debug))
  (put 'mode-line-format
       'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format nil))))

(set-language-environment "UTF-8")

(setq read-process-output-max
      (* 2 1024 1024))

(setq process-adaptive-read-buffering nil)

(setq ffap-machine-p-known 'reject)

(setq warning-minimum-level :error
      warning-suppress-types '((lexical-binding)))

(setq ad-redefinition-action 'accept)

(emacs--restore-gc-values)
