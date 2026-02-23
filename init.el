(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package emacs
  :demand t
  :ensure nil
  :init
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room
  (menu-bar-mode -1)          ; Disable the menu bar
  (set-face-attribute 'default nil :font "JBMono Nerd Font" :height 130)
  (set-fontset-font t nil (font-spec :size 20 :name "JBMono Nerd Font"))
  (setq-default line-spacing 0.2)
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :height 130))))
   '(fixed-pitch ((t ( :family "JBMono Nerd Font" :height 130)))))
  (add-to-list 'face-font-rescale-alist '("Atkinson Hyperlegible Next" . 1.2))
  (setopt inhibit-splash-screen t)
  (setopt initial-major-mode 'fundamental-mode)
  (setopt display-time-default-load-average nil)
  (setopt auto-revert-avoid-polling t)
  (setopt auto-revert-interval 5)
  (setopt auto-revert-check-vc-info t)
  (global-auto-revert-mode)
  (savehist-mode)
  (windmove-default-keybindings 'control)
  (setopt sentence-end-double-space nil)
  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))
  (defun dysthesis/backup-file-name (fpath)
    "Return a new file path of a given file path.
  If the new path's directories does not exist, create them."
    (let* ((backupRootDir "~/.config/emacs/emacs-backup/")
           (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
           (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
      (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
      backupFilePath))
  (setopt make-backup-file-name-function 'dysthesis/backup-file-name)
  (setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
  (setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
  (setopt completions-detailed t)                        ; Show annotations
  (setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates
  
  (setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setopt completions-max-height 20)                     ; This is arbitrary
  (setopt completions-detailed t)
  (setopt completions-format 'one-column)
  (setopt completions-group t)
  (setopt completion-auto-select 'second-tab)            ; Much more eager
  ;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values
  
  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell
  
  ;; For a fancier built-in completion option, try ido-mode,
  ;; icomplete-vertical, or fido-mode. See also the file extras/base.el
  
  ;(icomplete-vertical-mode)
  ;(fido-vertical-mode)
  ;(setopt icomplete-delay-completions-threshold 4000)
  ;; Mode line information
  (setopt line-number-mode t)                        ; Show current line in modeline
  (setq display-line-numbers-type 'relative)
  (setopt column-number-mode t)                      ; Show column as well
  
  (setopt x-underline-at-descent-line nil)           ; Prettier underlines
  (setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent
  
  (setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
  (setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
  
  ;; Enable horizontal scrolling
  (setopt mouse-wheel-tilt-scroll t)
  (setopt mouse-wheel-flip-direction t)
  
  ;; We won't set these, but they're good to know about
  ;;
  ;; (setopt indent-tabs-mode nil)
  ;; (setopt tab-width 4)
  
  ;; Misc. UI tweaks
  (blink-cursor-mode -1)                                ; Steady cursor
  (pixel-scroll-precision-mode)                         ; Smooth scrolling
  
  ;; Use common keystrokes by default
  (cua-mode)
  
  ;; Display line numbers in programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setopt display-line-numbers-width 3)           ; Set a minimum width
  
  ;; Nice line wrapping when working with text
  (add-hook 'text-mode-hook 'visual-line-mode)
  
  ;; Modes to highlight the current line with
  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))
  ;; Show the tab-bar as soon as tab-bar functions are invoked
  (setopt tab-bar-show 1)
  
  ;; Add the time to the tab-bar, if visible
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
  (setopt display-time-format "%a %F %T")
  (setopt display-time-interval 1)
  (display-time-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-bindings grid)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; Basically fuzzy finding
     orderless-flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package which-key
  :after (vertico)
  :ensure t
  :config
  (setq which-key-show-early-on-C-h t
      which-key-idle-delay 1e6 ; 11 days
      which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(use-package general
  :ensure t
  :after (evil)
  :demand t
  :config
  (general-evil-setup)
  ;; Set up 'SPC' as the leader key
  (general-create-definer dysthesis/start/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"           ;; Set leader key
    :global-prefix "C-SPC") ;; Set global leader key

  (dysthesis/start/leader-keys
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "p" '(:keymap projectile-command-map
                  :package projectile
                  :wk "Projectile command map"))

  (dysthesis/start/leader-keys
    "f" '(:ignore t :wk "Find")
    "f r" '(consult-recent-file :wk "Recent files")
    "f f" '(consult-fd :wk "Fd search for files")
    "f g" '(consult-ripgrep :wk "Ripgrep search in files")
    "f l" '(consult-line :wk "Find line")
    "f i" '(consult-imenu :wk "Imenu buffer locations"))

  (dysthesis/start/leader-keys
    "b" '(:ignore t :wk "Buffer Bookmarks")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b j" '(consult-bookmark :wk "Bookmark jump"))

  (dysthesis/start/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d v" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current"))

  (dysthesis/start/leader-keys
    "e" '(:ignore t :wk "Eglot Evaluate")
    "e e" '(eglot-reconnect :wk "Eglot Reconnect")
    "e f" '(eglot-format :wk "Eglot Format")
    "e l" '(consult-flymake :wk "Consult Flymake")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (dysthesis/start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g g" '(magit-status :wk "Magit status"))

  (dysthesis/start/leader-keys
    "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el"))
            :wk "Reload Emacs config"))

  (dysthesis/start/leader-keys
    "s" '(:ignore t :wk "Show"))

  (dysthesis/start/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Accept VCS markers as project root markers
  (setopt project-vc-extra-root-markers '(".projectile" ".git")))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.2)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
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
  ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) 
  ;; Keyword/Snipet completion
  (add-to-list 'completion-at-point-functions #'cape-keyword))

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

(use-package tempel-collection
  :ensure t
  :after tempel)

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  ;; ("M-h" 'sp-backward-slurp-sexp)
  ;; ("M-l" 'sp-forward-slurp-sexp)
  ;; ("M-H" 'sp-backward-barf-sexp)
  ;; ("M-L" 'sp-forward-barf-sexp)
  ;; ("M-r" '(sp-rewrap-sexp :wk "Change wrapping parentheses"))
  ;; ("C-M-t" 'sp-transpose-sexp)
  :config
  ;; Define keybindings with general.el
  (general-define-key
   :states 'insert
   "M-h" 'sp-backward-slurp-sexp)
  ;; load default config
  (require 'smartparens-config))

(use-package eglot
  :defer t
  :ensure nil
  :hook
  (prog-mode . (lambda ()
                 (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                   (eglot-ensure))))
  (eglot-managed-mode . +lsp-optimization-mode)
  :custom
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
  ;; NOTE: We disable eglot-auto-display-help-buffer because :select t in
  ;;   its popup rule causes eglot to steal focus too often.
  (eglot-auto-display-help-buffer nil)
  :config
  (dysthesis/start/leader-keys
   "c" '(:ignore t :which-key "Code")
   "c <escape>" '(keyboard-escape-quit :which-key t)
   "c r" '(eglot-rename :which-key "Rename")
   "c a" '(eglot-code-actions :which-key "Actions"))
  (with-eval-after-load 'eglot
    (dolist (mode '((nix-mode . ("nixd"))
                    ((rust-ts-mode rust-mode) . ("rust-analyzer"
  					       :initializationOptions (:check (:command "clippy"))))))
      (add-to-list 'eglot-server-programs mode)))
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'eglot-format nil t))))

(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :config
  (dysthesis/start/leader-keys
    "c s" '(consult-eglot-symbols :wk "Code Symbols")))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :after (transient)
  :config
  (dysthesis/start/leader-keys
    "g g" '(magit :wk "Magit")))

(use-package majutsu
  :ensure (:host github :repo "0WD0/majutsu")
  :config
  (dysthesis/start/leader-keys
  "g j" '(majutsu :wk "Majutsu")))

(use-package doric-themes
  :ensure t
  :demand t
  :config
  (doric-themes-select 'doric-obsidian))
(use-package solaire-mode
 :ensure t
 :config
 (solaire-global-mode +1))

(use-package olivetti
  :ensure t
  :config
  (defun dysthesis/org-mode-setup ()
    (org-indent-mode)
    (olivetti-mode)
    (display-line-numbers-mode 0)
    (olivetti-set-width 90))
  (add-hook 'org-mode-hook 'dysthesis/org-mode-setup))

(use-package mixed-pitch
  :ensure t
  :hook
  ;; You might want to enable it only in org-mode or both text-mode and org-mode
  ((org-mode) . mixed-pitch-mode)
  ((markdown-mode) . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch)
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-table
                  org-code
                  org-property-value
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
                  org-scheduled-previously))))

(use-package org
  :after (olivetti))

(use-package org-modern
  :ensure t
  :demand t
  :after (org)
  :config
  (set-face-background 'fringe (face-attribute 'default :background))
  (setq org-auto-align-tags nil
	org-tags-column 0
	org-modern-star 'replace
	org-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-agenda-tags-column 0
	org-ellipsis " ↪")
  (global-org-modern-mode))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))
