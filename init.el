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

(use-package general
  :demand t
  :config
  (general-create-definer start/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-evil-setup)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "." '(find-file :wk "Find file")
   "TAB" '(comment-line :wk "Comment lines")))

(use-package evil 
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t) ;; respect visual lines

  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t) ;; split windows created below
  (setq evil-vsplit-window-right t) ;; vertically split windows created to the right

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour

  (setq evil-undo-system 'undo-redo) ;; undo via 'u', and redo the undone change via 'C-r'; only available in emacs 28+.
  :config
  (evil-mode 1))

(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-h") #'evil-window-left)
(global-set-key (kbd "C-j") #'evil-window-down)
(global-set-key (kbd "C-k") #'evil-window-up)
(global-set-key (kbd "C-l") #'evil-window-right)

(use-package evil-collection ;; evilifies a bunch of things
  :ensure t
  :after evil
  :init
  (setq evil-collection-outline-bind-tab-p t) ;; '<TAB>' cycles visibility in 'outline-minor-mode'
  ;; If I want to incrementally enable evil-collection mode-by-mode, I can do something like the following:
  ;; (setq evil-collection-mode-list nil) ;; I don't like surprises
  ;; (add-to-list 'evil-collection-mode-list 'magit) ;; evilify magit
  ;; (add-to-list 'evil-collection-mode-list '(pdf pdf-view)) ;; evilify pdf-view
  :config
  (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode)) ;; globally enable evil-commentary

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)) ;; globally enable evil-surround

(use-package avy
  :ensure t
  :init
  (defun dysthesis/avy-action-insert-newline (pt)
    (save-excursion
      (goto-char pt)
      (newline))
    (select-window
     (cdr
      (ring-ref avy-ring 0))))
  (defun dysthesis/avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0))))
  (defun dysthesis/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t) ;; adds an avy action for embark
  :general
  (general-def '(normal motion)
    "s" 'evil-avy-goto-char-timer
    "f" 'evil-avy-goto-char-in-line
    "gl" 'evil-avy-goto-line ;; this rules
    ";" 'avy-resume)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'dysthesis/avy-action-embark ;; embark integration
        (alist-get ?i avy-dispatch-alist) 'dysthesis/avy-action-insert-newline
        (alist-get ?K avy-dispatch-alist) 'dysthesis/avy-action-kill-whole-line)) ;; kill lines with avy

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex                       ; Basically fuzzy finding
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "f f" '(consult-fd :wk "Fd")
   "f g" '(consult-ripgrep :wk "Ripgrep")
   "f l" '(consult-line :wk "Find line")
   "f i" '(consult-imenu :wk "Imenu"))
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
     ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
     ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
     ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
     ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
     ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

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

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook ((prog-mode text-mode markdown-mode) . smartparens-mode) ;; add `smartparens-mode` to these hooks
  :general
  ("M-h" 'sp-backward-slurp-sexp)
  ("M-l" 'sp-forward-slurp-sexp)
  ("M-H" 'sp-backward-barf-sexp)
  ("M-L" 'sp-forward-barf-sexp)
  ("M-r" '(sp-rewrap-sexp :wk "Change wrapping parentheses"))
  ("C-M-t" 'sp-transpose-sexp)
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

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

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers.  You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package dashboard
  :ensure t
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-banner-logo-title "Jormungandr")
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-startup-banner 'ascii)
  (dashboard-banner-ascii
   "                                          
                                          
                                           
   ⣴⣶⣤⡤⠦⣤⣀⣤⠆     ⣈⣭⣿⣶⣿⣦⣼⣆             
    ⠉⠻⢿⣿⠿⣿⣿⣶⣦⠤⠄⡠⢾⣿⣿⡿⠋⠉⠉⠻⣿⣿⡛⣦        
          ⠈⢿⣿⣟⠦ ⣾⣿⣿⣷    ⠻⠿⢿⣿⣧⣄         
           ⣸⣿⣿⢧ ⢻⠻⣿⣿⣷⣄⣀⠄⠢⣀⡀⠈⠙⠿⠄       
          ⢠⣿⣿⣿⠈    ⣻⣿⣿⣿⣿⣿⣿⣿⣛⣳⣤⣀⣀      
   ⢠⣧⣶⣥⡤⢄ ⣸⣿⣿⠘  ⢀⣴⣿⣿⡿⠛⣿⣿⣧⠈⢿⠿⠟⠛⠻⠿⠄  
  ⣰⣿⣿⠛⠻⣿⣿⡦⢹⣿⣷   ⢊⣿⣿⡏  ⢸⣿⣿⡇ ⢀⣠⣄⣾⠄    
 ⣠⣿⠿⠛ ⢀⣿⣿⣷⠘⢿⣿⣦⡀ ⢸⢿⣿⣿⣄ ⣸⣿⣿⡇⣪⣿⡿⠿⣿⣷⡄  
 ⠙⠃   ⣼⣿⡟  ⠈⠻⣿⣿⣦⣌⡇⠻⣿⣿⣷⣿⣿⣿ ⣿⣿⡇ ⠛⠻⢷⣄  
      ⢻⣿⣿⣄   ⠈⠻⣿⣿⣿⣷⣿⣿⣿⣿⣿⡟ ⠫⢿⣿⡆        
       ⠻⣿⣿⣿⣿⣶⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⡟⢀⣀⣤⣾⡿⠃       
")
:config
(dashboard-setup-startup-hook))

(use-package olivetti
  :ensure t
  :config
  (defun dysthesis/org-mode-setup ()
    (org-indent-mode)
    (olivetti-mode)
    (display-line-numbers-mode 0)
    (olivetti-set-width 90))
  (add-hook 'org-mode-hook 'dysthesis/org-mode-setup))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t))

(defun dysthesis/configure-font (&optional frame)
  "Configure font given initial non-daemon FRAME. Intended for `after-make-frame-functions'."
  (let ((my-font-height (if (string= (system-name) "deimos") 90 70))
	(my-font-size   (if (string= (system-name) "deimos") 9 7)))
    (set-face-attribute 'default nil :font "JBMono Nerd Font" :height my-font-height)
    (set-fontset-font t nil (font-spec :size my-font-size :name "JBMono Nerd Font"))
    (setq-default line-spacing 0.2)
    (custom-theme-set-faces
     'user
     `(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :height ,my-font-height))))
     `(fixed-pitch ((t (:family "JBMono Nerd Font" :height ,my-font-height))))))
  (add-to-list 'face-font-rescale-alist '("Atkinson Hyperlegible Next" . 1.16)) 
  (remove-hook 'after-make-frame-functions #'dysthesis/configure-font))

(add-hook 'after-make-frame-functions #'dysthesis/configure-font)
(unless (daemonp)
  (dysthesis/configure-font (selected-frame)))

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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

(use-package corfu
  ;; Optional customizations
  :ensure t
  :custom
  (corfu-popupinfo-mode 1)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.2)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; The functions that are added later will be the first in the list

  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  ;;(add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

;; Configure Tempel
(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
	 (:map tempel-map
	       ([backtab] . tempel-previous)
	       ([tab] . tempel-next)))
  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :ensure t
  :after tempel)

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :after (transient)
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "gg" '(magit :wk "Magit")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "p" '(projectile-command-map :wk "+ Projectile"))
  (projectile-mode)
  :init
  (setq projectile-project-search-path '("~/Documents/Projects"
                                         "~/Documents/University"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package tree-sitter-langs
  :ensure t)

(use-package evil-textobj-tree-sitter
  :ensure t
  :after (evil tree-sitter)
  :config
  (define-key evil-outer-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj ("function.outer")))
  (define-key evil-inner-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj("function.inner")))
  (define-key evil-inner-text-objects-map "i"
              (evil-textobj-tree-sitter-get-textobj ("parameter.inner")))
  (define-key evil-outer-text-objects-map "i"
              (evil-textobj-tree-sitter-get-textobj ("parameter.outer")))
  (define-key evil-outer-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj ("conditional.outer"
                                                     "loop.outer"))))

(use-package eglot
  :defer t
  :ensure nil
  :hook
  (prog-mode . (lambda ()
                 (unless (derived-mode-p 'emacs-lisp-mode
                                         'lisp-mode
                                         'makefile-mode
                                         'snippet-mode)
                   (eglot-ensure))))
  (eglot-managed-mode . +lsp-optimization-mode)
  :custom
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
  (eglot-auto-display-help-buffer nil) 
  (setq eglot-sync-connect 0
        eglot-autoshutdown t
        eglot-extend-to-xref t)
  
  :general
  (start/leader-keys
   "c" '(:ignore t :which-key "Code")
   "c <escape>" '(keyboard-escape-quit :which-key t)
   "c r" '(eglot-rename :which-key "Rename")
   "c a" '(eglot-code-actions :which-key "Actions"))
  :config
  (with-eval-after-load 'eglot
    (dolist (mode '((nix-mode . ("nixd"))
                    ((rust-ts-mode rust-mode) . ("rust-analyzer"
                                                 :initializationOptions
                                                 (:check (:command "clippy"))))))
      (add-to-list 'eglot-server-programs mode)))
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'eglot-format nil t))))

(use-package eglot-booster
  :ensure t
  :after eglot
  :config
  (eglot-booster-mode))

(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :general
  (start/leader-keys
	     "c s" '(consult-eglot-symbols :wk "Code Symbols")))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  (treesit-language-available-p 'rust)
  ;; (rust-mode-treesitter-derive t)
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . eldoc-mode)
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  ;; prettify symbols
  (rust-mode . (lambda () (prettify-symbols-mode))))
  (use-package cargo
    :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))

(use-package zig-mode
  :ensure t
  :after eglot
  :custom (zig-format-on-save 1)
  :hook
  (zig-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '(zig-mode . ((executable-find "zls")
 			     :initializationOptions
			     (:zig_exe_path (executable-find "zig")))))
  (if (>= emacs-major-version 28)
      (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
    (progn
      (defun colorize-compilation-buffer ()
     	(let ((inhibit-read-only t))
   	  (ansi-color-apply-on-region compilation-filter-start (point))))
      (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))))

(use-package envrc
  :ensure t
  :hook (after-init-hook . envrc-global-mode))

(setq org-directory "~/Org/")

(setq org-ellipsis " ↪")

(setq org-pretty-entities t)

(setq org-startup-folded t)

(custom-theme-set-faces
 'user
 '(org-level-1 ((t (:inherit default :height 1.5  :weight bold))))
 '(org-level-2 ((t (:inherit default :height 1.4  :weight bold))))
 '(org-level-3 ((t (:inherit default :height 1.3  :weight bold))))
 '(org-level-4 ((t (:inherit default :height 1.25 :weight semi-bold))))
 '(org-level-5 ((t (:inherit default :height 1.2  :weight normal))))
 '(org-level-6 ((t (:inherit default :height 1.15 :weight normal))))
 '(org-level-7 ((t (:inherit default :height 1.1  :weight normal))))
 '(org-level-8 ((t (:inherit default :height 1.05 :weight normal))))
 '(org-document-title ((t (:height 2.0 :weight heavy)))))

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

(setq org-modern-star 'replace)

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
