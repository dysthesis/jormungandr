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

(use-package meow
:demand t
;:bind (("C-w s" . split-window-below)
;       ("C-w v" . split-window-right)
;       ("C-w c" . delete-window))
:config
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
      meow-use-clipboard t)

(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))

(meow-leader-define-key
 ;; SPC j/k will run the original command in MOTION state.
 '("j" . "H-j")
 '("k" . "H-k")

 ;; Use SPC (0-9) for digit arguments.
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet))

(meow-normal-define-key
 '("0" . meow-expand-0)
 '("9" . meow-expand-9)
 '("8" . meow-expand-8)
 '("7" . meow-expand-7)
 '("6" . meow-expand-6)
 '("5" . meow-expand-5)
 '("4" . meow-expand-4)
 '("3" . meow-expand-3)
 '("2" . meow-expand-2)
 '("1" . meow-expand-1)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 '("a" . meow-append)
 '("A" . meow-open-below)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 '("d" . meow-delete)
 '("D" . meow-backward-delete)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("I" . meow-open-above)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . meow-search)
 '("o" . meow-block)
 '("O" . meow-to-block)
 '("p" . meow-yank)
 '("q" . meow-quit)
 '("Q" . meow-goto-line)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("s" . meow-kill)
 '("t" . meow-till)
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-visit)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-line)
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("'" . repeat)
 '("<escape>" . ignore))

;; Add support for arrows as a surround pair
(meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
(add-to-list 'meow-char-thing-table '(?a . arrow))
;; Meow!
(meow-global-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 4)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t))

(set-face-attribute 'default nil :font "JBMono Nerd Font" :height 90)
(set-fontset-font t nil (font-spec :size 10 :name "JBMono Nerd Font"))
(setq-default line-spacing 0.2)
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :height 90))))
 '(fixed-pitch ((t ( :family "JBMono Nerd Font" :height 90)))))

(use-package tao-theme
  :ensure t
  :demand t
  :config (load-theme 'tao-yin t))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(setq display-line-numbers-type 'relative)

(use-package mixed-pitch
  :hook
  ;; You might want to enable it only in org-mode or both text-mode and org-mode
  ((org-mode) . mixed-pitch-mode)
  ((markdown-mode) . mixed-pitch-mode)
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-table
                  org-code
                  org-block
                  org-block-begin-line
                  org-block-end-line
                  org-meta-line
                  org-document-info-keyword
                  org-tag
                  org-time-grid
                  org-todo
                  org-done
                  org-agenda-date
                  org-date
                  org-drawer
                  org-modern-tag
                  org-modern-done
                  org-modern-label
                  org-scheduled
                  org-scheduled-today
                  neo-file-link-face
                  org-scheduled-previously)))
  (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset))

(setq org-directory "~/Org/")

(setq org-ellipsis " ↪")

(setq org-pretty-entities t)

(setq org-startup-folded t)

(package-initialize)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq org-hide-emphasis-markers t)

(setq  org-modern-list
       '((42 . "•")
         (43 . "◈")
         (45 . "➤")))

(setq org-modern-block-name
      '((t . t)
        ("src" "»" "«")
        ("example" "»–" "–«")
        ("quote" "" "")
        ("export" "⏩" "⏪")))

(setq org-modern-block-fringe 6)

(when (require 'org-modern-indent nil 'noerror)
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(setq org-modern-keyword
 '((t . t)
   ("title" . "𝙏 ")
   ("filetags" . "󰓹 ")
   ("auto_tangle" . "󱋿 ")
   ("subtitle" . "𝙩 ")
   ("author" . "𝘼 ")
   ("email" . #(" " 0 1 (display (raise -0.14))))
   ("date" . "𝘿 ")
   ("property" . "☸ ")
   ("options" . "⌥ ")
   ("startup" . "⏻ ")
   ("macro" . "𝓜 ")
   ("bind" . #(" " 0 1 (display (raise -0.1))))
   ("bibliography" . " ")
   ("print_bibliography" . #(" " 0 1 (display (raise -0.1))))
   ("cite_export" . "⮭ ")
   ("print_glossary" . #("ᴬᶻ " 0 1 (display (raise -0.1))))
   ("glossary_sources" . #(" " 0 1 (display (raise -0.14))))
   ("include" . "⇤ ")
   ("setupfile" . "⇚ ")
   ("html_head" . "🅷 ")
   ("html" . "🅗 ")
   ("latex_class" . "🄻 ")
   ("latex_class_options" . #("🄻 " 1 2 (display (raise -0.14))))
   ("latex_header" . "🅻 ")
   ("latex_header_extra" . "🅻⁺ ")
   ("latex" . "🅛 ")
   ("beamer_theme" . "🄱 ")
   ("beamer_color_theme" . #("🄱 " 1 2 (display (raise -0.12))))
   ("beamer_font_theme" . "🄱𝐀 ")
   ("beamer_header" . "🅱 ")
   ("beamer" . "🅑 ")
   ("attr_latex" . "🄛 ")
   ("attr_html" . "🄗 ")
   ("attr_org" . "⒪ ")
   ("call" . #(" " 0 1 (display (raise -0.15))))
   ("name" . "⁍ ")
   ("header" . "› ")
   ("caption" . "☰ ")
   ("results" . "🠶")))

(setq org-agenda-tags-column 0
      org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")

(setq org-modern-todo-faces
 '(("WAIT"
    :inverse-video t
    :inherit +org-todo-onhold)
   ("NEXT"
    :inverse-video t
    :foreground "#89b4fa")
   ("PROG"
    :inverse-video t
    :foreground "#a6e3a1")
   ("TODO"
    :inverse-video t
    :foreground "#fab387")))

(use-package org-modern
  :demand t
  :config
  (global-org-modern-mode 1))
