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
    :states '(normal visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-evil-setup)
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "." '(find-file :wk "Find file")
   "TAB" '(comment-line :wk "Comment lines")))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode 1))

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

  (setq evil-undo-system 'undo-fu) ;; use undo-fu for a more predictable undo tree
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

(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (when (display-graphic-p)
    (vertico-posframe-mode 1)))

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
   :states '(normal visual emacs)
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

(use-package flymake
  :ensure nil
  :after (consult eglot)
  :general
  (start/leader-keys
   :keymaps 'flymake-mode-map
   "el" '(consult-flymake :wk "List errors"))
  :hook
  (prog-mode . flymake-mode)
  (flymake-mode . (lambda () (or (ignore-errors flymake-show-project-diagnostics)
                                 (flymake-show-buffer-diagnostics))))
  :custom
  (flymake-no-changes-timeout nil)
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-wrap-around nil)
  :general
  (general-nmap "en" 'flymake-goto-next-error)
  (general-nmap "ep" 'flymake-goto-prev-error))

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

(load-theme 'lackluster t)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "gg" '(magit :wk "Magit")))

(use-package diff-hl
  :ensure t
  :demand t
  :custom
  (diff-hl-global-modes '(not image-mode pdf-view-mode))
  (vc-git-diff-switches '("--histogram"))
  (diff-hl-flydiff-delay (if (eq system-type 'darwin) 1.0 0.5))
  (diff-hl-update-async (or (> emacs-major-version 30) 'thread))
  (diff-hl-show-staged-changes nil)
  (diff-hl-draw-borders nil)
  :hook (vc-dir-mode . turn-on-diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (dired-mode . dysthesis/vc-gutter-enable-maybe-h)
  :init
  (defun dysthesis/vc-gutter-enable-maybe-h ()
    "Conditionally enable `diff-hl-dired-mode' in Dired buffers.
Respects `diff-hl-disable-on-remote'."
    (require 'diff-hl-dired nil t)
    (when (fboundp 'diff-hl-dired-mode)
      (unless (and (bound-and-true-p diff-hl-disable-on-remote)
                   (file-remote-p default-directory))
        (diff-hl-dired-mode 1))))
  :config
  (require 'cl-lib)
  (if (fboundp 'fringe-mode) (fringe-mode 8))
  (setq-default fringes-outside-margins t)

  ;; Sleeker, thinner fringe bitmaps (From Doom's `vc-gutter` module).
  (defun dysthesis/vc-gutter-define-thin-bitmaps-a (&rest _)
    (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                           (numberp text-scale-mode-amount))
                      (expt text-scale-mode-step text-scale-mode-amount)
                    1))
           (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
           (h (+ (ceiling (* (frame-char-height) scale))
                 (if (floatp spacing)
                     (truncate (* (frame-char-height) spacing))
                   spacing)))
           (bmp-max-width (if (boundp 'diff-hl-bmp-max-width) diff-hl-bmp-max-width 16))
           (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                   bmp-max-width))
           (_ (if (zerop w) (setq w bmp-max-width))))

      (define-fringe-bitmap 'diff-hl-bmp-middle
        (make-vector
         h (string-to-number (let ((half-w (1- (/ w 2))))
                               (concat (make-string half-w ?1)
                                       (make-string (- w half-w) ?0)))
                             2))
        nil nil 'center)))

  (advice-add 'diff-hl-define-bitmaps :after #'dysthesis/vc-gutter-define-thin-bitmaps-a)

  (defun dysthesis/vc-gutter-type-at-pos-fn (type _pos)
    (if (eq type 'delete) 'diff-hl-bmp-delete 'diff-hl-bmp-middle))

  (setq diff-hl-fringe-bmp-function #'dysthesis/vc-gutter-type-at-pos-fn)

  (defun dysthesis/vc-gutter-make-diff-hl-faces-transparent-h (&rest _)
    (dolist (face '(diff-hl-insert diff-hl-delete diff-hl-change))
      (set-face-background face nil)))
  (add-hook 'diff-hl-mode-hook #'dysthesis/vc-gutter-make-diff-hl-faces-transparent-h)
  (add-hook 'after-make-frame-functions #'dysthesis/vc-gutter-make-diff-hl-faces-transparent-h)
  (add-hook 'enable-theme-functions #'dysthesis/vc-gutter-make-diff-hl-faces-transparent-h)

  ;; FIX: Let diff-hl keep the left fringe (if Flycheck is used).
  (with-eval-after-load 'flycheck
    (setq flycheck-indication-mode 'right-fringe)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; PERF: Suppress TRAMP's unsafe temp file prompts (Doom's workaround).
  (defun dysthesis/vc-gutter--silence-temp-file-prompts-a (fn &rest args)
    (let ((tramp-allow-unsafe-temporary-files t))
      (apply fn args)))
  (advice-add 'diff-hl-diff-buffer-with-reference
              :around #'dysthesis/vc-gutter--silence-temp-file-prompts-a)

  ;; Update diffs when Magit refreshes.
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  ;; Better keys in the hunk popup (Evil).
  (with-eval-after-load 'diff-hl-show-hunk
    (when (bound-and-true-p evil-mode)
      (general-define-key
       :states '(normal)
       :keymaps 'diff-hl-show-hunk-map
       "p" #'diff-hl-show-hunk-previous
       "n" #'diff-hl-show-hunk-next
       "c" #'diff-hl-show-hunk-copy-original-text
       "r" #'diff-hl-show-hunk-revert-hunk
       "[" #'diff-hl-show-hunk-previous
       "]" #'diff-hl-show-hunk-next
       "{" #'diff-hl-show-hunk-previous
       "}" #'diff-hl-show-hunk-next)
      (let ((stage-fn (or (and (fboundp 'diff-hl-show-hunk-stage-hunk)
                               #'diff-hl-show-hunk-stage-hunk)
                          (and (fboundp 'diff-hl-stage-current-hunk)
                               #'diff-hl-stage-current-hunk))))
        (when stage-fn
          (general-define-key
           :states '(normal)
           :keymaps 'diff-hl-show-hunk-map
           "S" stage-fn)))))

  ;; UX: Refresh gutter when changing windows or refocusing the frame.
  (defvar-local dysthesis/vc-gutter--last-state nil)
  (defun dysthesis/vc-gutter-update-h (&rest _)
    (when (or (bound-and-true-p diff-hl-mode)
              (bound-and-true-p diff-hl-dir-mode))
      (let ((file (buffer-file-name (buffer-base-buffer))))
        (when file
          (let* ((props
                  (when (boundp 'vc-file-prop-obarray)
                    (copy-sequence
                     (symbol-plist
                      (intern (expand-file-name file)
                              vc-file-prop-obarray)))))
                 (state (cons (point) props)))
            (unless (equal dysthesis/vc-gutter--last-state
                           (setq dysthesis/vc-gutter--last-state state))
              (ignore (diff-hl-update))))))))
  (when (boundp 'window-selection-change-functions)
    (add-hook 'window-selection-change-functions #'dysthesis/vc-gutter-update-h t))
  (add-hook 'focus-in-hook #'dysthesis/vc-gutter-update-h t)

  ;; Keep point stable when reverting hunks.
  (defun dysthesis/vc-gutter--save-excursion-a (fn &rest args)
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt))))
  (advice-add 'diff-hl-revert-hunk :around #'dysthesis/vc-gutter--save-excursion-a)

  ;; Shrink the revert window to its contents.
  (defun dysthesis/vc-gutter--shrink-popup-a (fn &rest args)
    (let* ((refine-mode diff-auto-refine-mode)
           (diff-auto-refine-mode t)
           (orig (symbol-function 'diff-refine-hunk)))
      (cl-letf (((symbol-function 'diff-refine-hunk)
                 (lambda ()
                   (when refine-mode
                     (funcall orig))
                   (shrink-window-if-larger-than-buffer))))
        (apply fn args))))
  (advice-add 'diff-hl-revert-hunk-1 :around #'dysthesis/vc-gutter--shrink-popup-a)

  ;; Update diff-hl immediately upon exiting insert mode.
  (defun dysthesis/vc-gutter-init-flydiff-mode-h ()
    (when (bound-and-true-p evil-mode)
      (if diff-hl-flydiff-mode
          (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
        (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update))))
  (add-hook 'diff-hl-flydiff-mode-hook #'dysthesis/vc-gutter-init-flydiff-mode-h)

  (global-diff-hl-mode 1))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (general-define-key
   :states '(normal visual emacs)
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

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gc-cons-threshold nil)
(defvar +lsp--default-gc-cons-percentage nil)

(define-minor-mode +lsp-optimization-mode
  "Deploy small GC and IPC optimisations for Eglot."
  :global t
  :init-value nil
  (if +lsp-optimization-mode
      (progn
        (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max))
        (setq +lsp--default-gc-cons-threshold (default-value 'gc-cons-threshold))
        (setq +lsp--default-gc-cons-percentage (default-value 'gc-cons-percentage))

        (setq-default read-process-output-max (* 2 1024 1024))
        (setq-default gc-cons-threshold (* 128 1024 1024))
        (setq-default gc-cons-percentage 0.6))
    (when (numberp +lsp--default-read-process-output-max)
      (setq-default read-process-output-max +lsp--default-read-process-output-max))
    (when (numberp +lsp--default-gc-cons-threshold)
      (setq-default gc-cons-threshold +lsp--default-gc-cons-threshold))
    (when (numberp +lsp--default-gc-cons-percentage)
      (setq-default gc-cons-percentage +lsp--default-gc-cons-percentage))))

(use-package eglot
  :defer t
  :ensure nil
  :hook
  (prog-mode . (lambda ()
                 (unless (derived-mode-p 'emacs-lisp-mode
                                         'lisp-mode
                                         'makefile-mode
                                         'snippet-mode
                                         'lean4-mode)
                   (eglot-ensure))))
  (eglot-managed-mode . (lambda ()
                          (+lsp-optimization-mode (if eglot-managed-mode 1 -1))))
  :custom
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-auto-display-help-buffer nil)
  :init
  (setq jsonrpc-event-hook nil)
  (when (boundp 'eglot-events-buffer-size)
    (setq eglot-events-buffer-size 0))
  (setq eglot-events-buffer-config '(:size 0 :format short))
  
  (setq eglot-report-progress nil)
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

(use-package eldoc-box
  :ensure t
  :after (eldoc eglot)
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(defcustom dysthesis/codelldb-path (executable-find "codelldb")
  "Path to the codelldb installation."
  :type 'string)

(use-package dape
  :ensure t
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-cwd-function 'projectile-project-root)
  :config
  (add-hook 'dape-compile-hook 'kill-buffer)
  (add-to-list 'dape-configs
               `(codelldb-rust
                 modes (rust-mode rust-ts-mode)
                 command dysthesis/codelldb-path
                 :type "lldb"
                 :request "launch"
                 command-args ("--port" :autoport "--settings" "{\"sourceLanguages\":[\"rust\"]}")
                 ensure dape-ensure-command port :autoport fn dape-config-autoport
                 :cwd dape-cwd-fn
                 :program (lambda ()
                            (let* ((project-root (dape-cwd))
                                   (project-name (file-name-nondirectory
                                                  (directory-file-name project-root))))
                              (file-name-concat "target" "debug" project-name)))
                 :args [])))

(use-package repeat
  :ensure nil
  :config
  (repeat-mode 1))

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

(use-package haskell-mode
  :ensure t)

(use-package lean4-mode
  :ensure nil
  :mode "\\.lean\\'"
  :hook (lean4-mode . lsp-deferred))

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

(use-package python
  :ensure nil
  :after eglot
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :init
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")))

(use-package anaconda-mode
  :ensure t
  :defer t
  :after python
  :init
  (setq anaconda-mode-eldoc-as-single-line t)
  :config
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "g d" 'anaconda-mode-find-definitions
   "g h" 'anaconda-mode-show-doc
   "g a" 'anaconda-mode-find-assignments
   "g f" 'anaconda-mode-find-file
   "g u" 'anaconda-mode-find-references))

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

(defvar dysthesis/bibliography-files '("~/Library/Library.bib")
  "List of bibliography files used by citar and org-cite.")

(use-package citar
  :ensure t
  :demand t
  :custom
  (citar-bibliography dysthesis/bibliography-files)
  :hook
  ((org-mode LaTeX-mode) . citar-capf-setup)
  :general
  ("C-c o" 'citar-open)
  ("C-c b" 'org-cite-insert))

(require 'cl-lib)
(require 'subr-x)

(use-package org
  :ensure nil
  :general
  ("C-c c" 'org-capture)
  ("S-RET" 'org-open-at-point)
  :custom
  (org-archive-location (concat org-directory "archive.org::* From =%s="))
  (org-preview-latex-default-process 'dvisvgm)
  (org-highlight-latex-and-related '(latex script entities))
  (org-cite-global-bibliography dysthesis/bibliography-files)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(defun dysthesis/agenda ()
  (interactive)
  (org-agenda nil "o"))

(use-package org-agenda
  :ensure nil
  :after org
  :general
  ("C-c a" 'dysthesis/agenda)
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "PROG(p)" "|" "DONE(d)" "|" "CANCEL(c)")))
  (org-agenda-sorting-strategy
   '((urgency-up deadline-up priority-down effort-up)))
  (org-agenda-start-day "0d")
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-block-separator nil)
  :config
  (let ((gtd-dir (expand-file-name "GTD/" org-directory)))
    (setq org-agenda-files
          (if (file-directory-p gtd-dir)
              (directory-files-recursively gtd-dir "\\.org$")
            nil)))
  (setq org-refile-targets
        `((,(expand-file-name "GTD/gtd.org" org-directory) :maxlevel . 2)
          (,(expand-file-name "GTD/someday.org" org-directory) :maxlevel . 2)
          (,(expand-file-name "GTD/tickler.org" org-directory) :maxlevel . 2)
          (,(expand-file-name "GTD/routine.org" org-directory) :maxlevel . 2)
          (,(expand-file-name "GTD/reading.org" org-directory) :maxlevel . 2))))

(defun dysthesis/mark-inbox-todos ()
  "Mark entries in the agenda whose category is inbox for future bulk action."
  (let ((entries-marked 0)
        (regexp "inbox")
        category-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (while (re-search-forward regexp nil t)
        (setq category-at-point (get-text-property (match-beginning 0) 'org-category))
        (if (or (get-char-property (point) 'invisible)
                (not category-at-point))
            (beginning-of-line 2)
          (when (string-match-p regexp category-at-point)
            (setq entries-marked (1+ entries-marked))
            (call-interactively 'org-agenda-bulk-mark))))
      (unless entries-marked
        (message "No entry matching 'inbox'.")))))

(defun dysthesis/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)

   (let* ((hdmarker (org-get-at-bol 'org-hd-marker))
          (category (completing-read "Category: "
                                     '("University" "Home" "Tinkering" "Read"))))
     (with-current-buffer (marker-buffer hdmarker)
       (goto-char (marker-position hdmarker))
       (org-set-property "CATEGORY" category))

     (call-interactively 'dysthesis/my-org-agenda-set-effort)
     (org-agenda-refile nil nil t))))

(defvar dysthesis/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun dysthesis/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "EFFORT [%s]: " dysthesis/org-current-effort)
                      nil nil dysthesis/org-current-effort)))
  (setq dysthesis/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-fold-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil dysthesis/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun dysthesis/bulk-process-entries ()
  (when org-agenda-bulk-marked-entries
    (let ((entries (reverse org-agenda-bulk-marked-entries))
          (processed 0)
          (skipped 0))
      (dolist (e entries)
        (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
          (if (not pos)
              (progn (message "Skipping removed entry at %s" e)
                     (cl-incf skipped))
            (goto-char pos)
            (let (org-loop-over-headlines-in-active-region)
              (funcall 'dysthesis/org-agenda-process-inbox-item))
            (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                      (memq 'org-add-log-note post-command-hook))
              (org-add-log-note))
            (cl-incf processed))))
      (org-agenda-redo)
      (unless org-agenda-persistent-marks
        (org-agenda-bulk-unmark-all))
      (message "Acted on %d entries%s%s"
               processed
               (if (= skipped 0)
                   ""
                 (format ", skipped %d (disappeared before their turn)"
                         skipped))
               (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun dysthesis/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (dysthesis/mark-inbox-todos)
  (dysthesis/bulk-process-entries))

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(defun log-todo-next-creation-date (&rest _ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'."
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :custom
  (org-super-agenda-keep-order t)
  (org-agenda-custom-commands
   '(("o" "Overview"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :deadline today
                             :scheduled today
                             :order 0)
                      (:habit t
                              :order 1)
                      (:name "Overdue"
                             :deadline past
                             :scheduled past
                             :order 2)
                      (:name "Upcoming"
                             :and (:deadline future
                                             :priority>= "B")
                             :and (:scheduled future
                                              :priority>= "B")
                             :order 3)
                      (:discard (:anything t))))))
       (alltodo "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '((:name "Ongoing"
                              :todo "PROG"
                              :order 0)
                       (:name "Up next"
                              :todo "NEXT"
                              :order 1)
                       (:name "Waiting"
                              :todo "WAIT"
                              :order 2)
                       (:name "Important"
                              :priority "A"
                              :order 3)
                       (:name "Inbox"
                              :file-path "inbox"
                              :order 4)
                       (:name "University"
                              :category "University"
                              :tag ("university" "uni" "assignment" "exam")
                              :order 5)
                       (:name "Tinkering"
                              :category "Tinkering"
                              :tag ("nix" "nixos" "voidlinux" "neovim" "gentoo" "emacs" "tinker")
                              :order 6)
                       (:name "Reading list"
                              :category "Read"
                              :tag "read"
                              :order 6)))))))))
  :config
  (let ((inhibit-message t))
    (org-super-agenda-mode)))

(use-package doct
  :ensure t
  :commands (doct)
  :init
  (setq org-capture-templates
        (doct '((" Todo"
                 :keys "t"
                 :prepend t
                 :file "GTD/inbox.org"
                 :headline "Tasks"
                 :type entry
                 :template ("* TODO %? %{extra}")
                 :children ((" General"
                             :keys "t"
                             :extra "")
                            ("󰈸 With deadline"
                             :keys "d"
                             :extra "\nDEADLINE: %^{Deadline:}t")
                            ("󰥔 With schedule"
                             :keys "s"
                             :extra "\nSCHEDULED: %^{Start time:}t")))
                ("Bookmark"
                 :keys "b"
                 :prepend t
                 :file "bookmarks.org"
                 :type entry
                 :template "* TODO [[%:link][%:description]] :bookmark:\n\n"
                 :immediate-finish t)))))

(defun +org--toggle-inline-images-in-subtree (&optional beg end)
  "Toggle inline images between BEG and END."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (if org-inline-image-overlays
        (org-remove-inline-images beg end)
      (org-display-inline-images nil nil beg end))))

(defun +org-get-todo-keywords-for (_keyword)
  "Return a reasonable default list of TODO keywords for the current buffer."
  (or (bound-and-true-p org-todo-keywords-1) '("TODO")))

(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (if (fboundp 'org--latex-preview-region)
                 (org--latex-preview-region beg end)
               (org-latex-preview arg)))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (org-toggle-checkbox))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (with-eval-after-load 'evil-org
    (define-key org-mode-map (kbd "<normal-state> RET") '+org/dwim-at-point))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename (expand-file-name "Roam/" org-directory)))
  (org-roam-complete-everywhere t)
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (org-roam-link-use-custom-faces 'everywhere)
  (org-roam-capture-templates
   '(("d" " Default" plain
      "%?"
      :if-new (file+head "${slug}.org"
                         "#+title: ${title}\n#+STARTUP: latexpreview")
      :immediate-finish t
      :unnarrowed t)
     ("i" "󰆼 Index note" plain
      "%?"
      :if-new (file+head "${slug}.org"
                         "#+title: ${title}\n#+filetags: :new:index:")
      :immediate-finish t
      :unarrowed t)
     ("e" "󰖟 Elfeed" plain
      "%?"
      :target (file+head "Elfeed/${slug}.org"
                         "#+title: ${title}\n#+filetags: :new:article:rss:\n#+STARTUP: latexpreview")
      :unnarrowed t)
     ("l" "󰙨 Literature note" plain
      "%?"
      :target
      (file+head
       "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/Literature/${citar-citekey}.org"
       "#+title: ${note-title}.\n#+filetags: :new:\n#+created: %U\n#+last_modified: %U\n#+STARTUP: latexpreview\n#+url: ${citar-howpublished}\n\n* Annotations\n:PROPERTIES:\n:Custom_ID: ${citar-citekey}\n:NOTER_DOCUMENT: ${citar-file}\n:NOTER_PAGE: \n:END:\n\n")
      :unnarrowed t)
     ("I" " Idea" plain "%?"
      :if-new
      (file+head "${slug}.org"
                 "#+title: ${title}\n#+filetags: :idea:new:\n#+STARTUP: latexpreview\n")
      :immediate-finish t
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M> %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-dailies-directory "Daily/")
  :general
  ("C-c n" '(:ignore t
                     :wk "Org-roam"))
  ("C-c n l" '(org-roam-buffer-toggle
               :wk "Toggle org-roam buffer"))
  ("C-c n f" '(org-roam-node-find
               :wk "Find org-roam node"))
  ("C-c n d" '(:keymap org-roam-dailies-map
                       :package org-roam
                       :wk "Org-roam dailies"))
  ("C-c n i" '(org-roam-node-insert
               :wk "Insert org-roam node"))
  ("C-c n c" '(org-roam-capture
               :wk "Capture into org-roam node"))
  ("C-c n t" '(org-roam-tag-add :wk "Add tag to current org-roam node"))
  ("C-c n a" '(org-roam-alias-add :wk "Add alias to current org-roam node"))
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package citar-org-roam
  :ensure t
  :after (:any citar org org-roam)
  :custom
  (citar-org-roam-capture-template-key "l")
  (citar-org-roam-note-title-template "${author} - ${title}")
  (citar-org-roam-template-fields
   '((:citar-title . ("title"))
     (:citar-author . ("author" "editor"))
     (:citar-date . ("date" "year" "issued"))
     (:citar-pages . ("pages"))
     (:citar-type . ("=type="))
     (:citar-file . ("file"))
     (:citar-howpublished . ("howpublished"))))
  :config
  (citar-org-roam-mode 1))

(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :config
  (org-roam-timestamps-mode))

(use-package org-ref
  :ensure t
  :after org
  :custom
  (org-ref-default-bibliography dysthesis/bibliography-files)
  (org-ref-pdf-directory "~/Library/Files")
  (org-ref-notes-directory (expand-file-name "Roam/Literature/" org-directory)))

(use-package org-noter
  :ensure t
  :after org
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-hide-other nil)
  (org-noter-notes-search-path (list org-directory)))

(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(plist-put org-format-latex-options :foreground "White")
(plist-put org-format-latex-options :background nil)
(plist-put org-format-latex-options :scale 0.65)

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode))

(add-to-list 'org-latex-packages-alist
             '("" "tikz" t))

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

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

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode))

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

(use-package elfeed
  :ensure t
  :after evil
  :bind ("C-c e" . elfeed)
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://matklad.github.io/feed.xml"
          "https://xeiaso.net/blog.rss"
          "https://sthbrx.github.io/rss.xml"
          "https://morss.it/without.boats/index.xml"
          "https://kristoff.it/index.xml"
          "https://fasterthanli.me/index.xml"
          "https://ayats.org/feed.xml"
          "https://morss.it/jeremykun.com/feed/"
          "https://morss.it/mitchellh.com/feed.xml"
          "https://blog.regehr.org/feed"
          "https://lobste.rs/t/rust,zig,c,osdev,nix,emacs,vim,security,performance,plt,distributed,cryptography,hardware,science,c++,haskell,elixir,linux,android,privacy,reversing,virtualization,compilers,editors,vcs.rss"
          "https://hnrss.org/frontpage"
          "https://mmapped.blog/feed.xml"
          "https://smallcultfollowing.com/babysteps//atom.xml"
          "https://justine.lol/rss.xml"
          "https://blog.cryptographyengineering.com/feed/"
          "https://steveklabnik.com/writing"
          "https://awesomekling.github.io/feed.xml"
          "https://dataswamp.org/~solene/rss.xml"
          "https://blogsystem5.substack.com/feed"
          "https://scottaaronson.blog/"
          "https://eli.thegreenplace.net/feeds/all.atom.xml"
          "https://lemire.me/blog/feed/"
          "https://syst3mfailure.io/feed.xml"
          "https://mcyoung.xyz/feed"
          "https://eli.thegreenplace.net/feeds/all.atom.xml"))
  (setq elfeed-search-filter "@2-week-ago +unread"))

(with-eval-after-load 'elfeed
  (evil-set-initial-state 'elfeed-search-mode 'normal)
  (evil-set-initial-state 'elfeed-show-mode   'normal))

(defun dysthesis/elfeed-capture-entry ()
  (interactive)
  (if (eq major-mode 'elfeed-show-mode)
      (let* ((entry elfeed-show-entry)
             (link (elfeed-entry-link entry))
             (title (elfeed-entry-title entry)))
        (if (and (fboundp 'org-roam-capture-)
                 (fboundp 'org-roam-node-create))
            (progn
              (org-roam-capture- :keys "e"
                                :node (org-roam-node-create :title title))
              (insert link))
          (message "Org-roam capture is unavailable.")))
    (message "Not in elfeed-show mode!")))

(with-eval-after-load 'elfeed-search
  (evil-define-key 'normal elfeed-search-mode-map
    "q" #'quit-window
    "g" #'elfeed-update
    "u" #'elfeed-search-tag-all-unread
    "r" #'elfeed-search-untag-all-unread
    "s" #'elfeed-search-live-filter
    (kbd "RET") #'elfeed-search-show-entry
    "b" #'elfeed-search-browse-url
    "y" #'elfeed-search-yank))

(with-eval-after-load 'elfeed-show
  (evil-define-key 'normal elfeed-show-mode-map
    "q" #'quit-window
    "n" #'elfeed-show-next
    "p" #'elfeed-show-prev
    "r" #'elfeed-show-refresh
    "b" #'elfeed-show-visit
    "y" #'elfeed-show-yank
    "c" #'dysthesis/elfeed-capture-entry))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun dysthesis/nov-mode-setup ()
    "Tweak nov-mode to be comfortable for long-form reading."
    (face-remap-add-relative 'variable-pitch
                             :family "Georgia Pro"
                             :height 1.4)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (when (require 'visual-fill-column nil t)
      (setq-local visual-fill-column-center-text t
                  visual-fill-column-width 81
                  nov-text-width 80)
      (visual-fill-column-mode 1))
    (hl-line-mode -1))
  (add-hook 'nov-mode-hook #'dysthesis/nov-mode-setup))

(use-package eat
  :ensure t
  :custom
  (eat-enable-auto-line-mode t)
  (eat-enable-yank-to-terminal t)
  (eat-enable-kill-from-terminal t)
  :bind (("C-x E" . eat)
         :map project-prefix-map
         ("t" . eat-project)))

(setq-default shell-file-name (executable-find "bash"))

(use-package async
  :ensure t
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(use-package ob-async
  :ensure t)

(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("FIXME" error bold)
                           ("HACK" font-lock-constant-face)
                           ("PERF" font-lock-constant-face)
                           ("NOTE" success bold)
                           ("REVIEW" font-lock-keyword-face bold)
                           ("DEPRECATED" font-lock-doc-face bold))))

(use-package hide-mode-line
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :hook (pdf-view-mode . hide-mode-line-mode)
  :custom
  (pdf-view-midnight-colors '("#ffffff" . "#080808"))
  :config
  (pdf-tools-install))
