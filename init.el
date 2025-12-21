(setq native-comp-async-query-on-exit t)

(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

(setq enable-recursive-minibuffers t)

(setq minibuffer-prompt-properties
      '(read-only t
        intangible t
        cursor-intangible t
        face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq which-func-update-delay 1.0)
(when (boundp 'idle-update-delay)
  (setq idle-update-delay which-func-update-delay))

(defalias #'view-hello-file #'ignore)

(setq visible-bell nil
      ring-bell-function #'ignore)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(setq custom-buffer-done-kill t)

(setq whitespace-line-column nil)

(setq-default display-line-numbers-width 3
              display-line-numbers-widen t)

(setq truncate-string-ellipsis "…")

(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq x-underline-at-descent-line t)

(setq remote-file-name-inhibit-cache 50)

(setq imenu-auto-rescan t)

(setq imenu-max-item-length 160)

(setq next-line-add-newlines nil)

(setq bookmark-save-flag 1)

(setq tramp-verbose 1
      tramp-completion-reread-directory-timeout 50)

(setq delete-by-moving-to-trash (not noninteractive)
      remote-file-name-inhibit-delete-by-moving-to-trash t)

(setq find-file-suppress-same-file-warnings t)

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq split-width-threshold 170
      split-height-threshold nil)

(setq uniquify-buffer-name-style 'forward)

(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(setq confirm-nonexistent-file-or-buffer nil)

(setq create-lockfiles nil
      make-backup-files nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)

(setq vc-git-print-log-follow t
      vc-make-backup-files nil
      vc-git-diff-switches '("--histogram"))

(setq auto-save-default nil
      auto-save-no-message t
      auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

(setq kill-buffer-delete-auto-save-files t)

(setq kill-do-not-save-duplicates t)

(setq revert-without-query (list ".")
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

(setq global-auto-revert-non-file-buffers t
      global-auto-revert-ignore-modes '(Buffer-menu-mode))

(setq recentf-max-saved-items 300
      recentf-max-menu-items 15
      recentf-auto-cleanup 'mode
      recentf-exclude nil)

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-limit 600)

(setq history-length 300
      savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring
        register-alist
        mark-ring global-mark-ring
        search-ring regexp-search-ring))

(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

(setq redisplay-skip-fontification-on-input t)

(setq fast-but-imprecise-scrolling t)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 20)
(setq auto-window-vscroll nil)
(setq scroll-margin 0)
(setq next-screen-context-lines 0)

(setq hscroll-margin 2
      hscroll-step 1)

(setq mouse-yank-at-point nil)

(when (and (display-graphic-p) (fboundp 'context-menu-mode))
  (add-hook 'after-init-hook #'context-menu-mode))

(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

(setq blink-matching-paren nil)

(setq x-stretch-cursor nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq global-text-scale-adjust-resizes-frames nil)
(setq delete-pair-blink-delay 0.03)

(setq-default left-fringe-width 8
              right-fringe-width 8
              indicate-buffer-boundaries nil
              indicate-empty-lines nil)

(setq-default word-wrap t
              truncate-lines t)
(setq truncate-partial-width-windows nil)

(setq-default electric-indent-chars '(?\n ?\^?)
              indent-tabs-mode nil
              tab-width 4)

(setq tab-always-indent 'complete)
(setq tab-first-completion 'word-or-paren-or-punct)

(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq comment-multi-line t
      comment-empty-lines t)

(setq-default fill-column 80)

(setq sentence-end-double-space nil)
(setq require-final-newline t)

(setq lazy-highlight-initial-delay 0)

(setq display-time-default-load-average nil)

(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)

(setq dired-free-space nil
      dired-dwim-target t
      dired-deletion-confirmer 'y-or-n-p
      dired-filter-verbose nil
      dired-recursive-deletes 'top
      dired-recursive-copies 'always
      dired-vc-rename-file t
      dired-create-destination-dirs 'ask
      dired-clean-confirm-killing-deleted-buffers nil)

(setq auto-revert-remote-files nil
      dired-auto-revert-buffer 'dired-buffer-stale-p)

(setq dired-omit-verbose nil
      dired-omit-files (concat "\\`[.]\\'"))

(setq ls-lisp-verbosity nil
      ls-lisp-dirs-first t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(setq apropos-do-all t)

(setq help-enable-completion-autoload nil
      help-enable-autoload nil
      help-enable-symbol-autoload nil
      help-window-select t)

(setq eglot-sync-connect 0
      eglot-autoshutdown t
      eglot-extend-to-xref t)

(setq jsonrpc-event-hook nil)
(when (boundp 'eglot-events-buffer-size)
  (setq eglot-events-buffer-size 0))
(setq eglot-events-buffer-config '(:size 0 :format short))

(setq eglot-report-progress nil)

(setq flymake-show-diagnostics-at-end-of-line nil
      flymake-wrap-around nil)

(setq hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil)

(setq icomplete-compute-delay 0.01)

(setq flyspell-issue-welcome-flag nil
      flyspell-issue-message-flag nil)

(setq text-mode-ispell-word-completion nil)

(setq ispell-silently-savep t)

(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 55 55 :left :elide)
              " " (size 8 -1 :right)
              " " (mode 18 18 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(setq xref-show-definitions-function 'xref-show-definitions-completing-read
      xref-show-xrefs-function 'xref-show-definitions-completing-read)

(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(setq save-abbrevs 'silently)

(setq dabbrev-upcase-means-case-search t)

(setq dabbrev-ignored-buffer-modes
      '(archive-mode image-mode docview-mode tags-table-mode
                     pdf-view-mode tags-table-mode))

(setq dabbrev-ignored-buffer-regexps
      '("\\` "
        "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))

(dolist (cmd '(list-timers narrow-to-region narrow-to-page
                           upcase-region downcase-region
                           list-threads erase-buffer scroll-left
                           dired-find-alternate-file set-goal-column))
  (put cmd 'disabled nil))
