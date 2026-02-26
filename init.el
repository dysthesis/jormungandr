(defconst dysthesis/immutable-config (getenv "JORMUNGANDR_IMMUTABLE"))

(defconst dysthesis/state-dir user-emacs-directory)
(defconst dysthesis/state-cache (expand-file-name "cache" dysthesis/state-dir))
(make-directory dysthesis/state-cache t)
(setq savehist-file (expand-file-name "history" dysthesis/state-cache)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" dysthesis/state-dir))

(use-package use-package
  :ensure t
  :custom
  ;; Don't automatically defer
  (use-package-always-defer nil)
  ;; Report loading details
  (use-package-verbose t)
  ;; This is really helpful for profiling
  (use-package-minimum-reported-time 0)
  ;; Expand normally
  (use-package-expand-minimally nil)
  ;; Navigate use-package declarations w/imenu
  (use-package-enable-imenu-support t))

;; Avoid runtime byte-compilation when immutability is requested.
(unless dysthesis/immutable-config
  (use-package auto-compile
    :ensure t
    :defer 1
    :custom
    (auto-compile-display-buffer nil)
    (auto-compile-mode-line-counter nil)
    (auto-compile-use-mode-line nil)
    (auto-compile-update-autoloads t)
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

(use-package gcmh
  :ensure t
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10)
  (gcmh-mode 1))

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
(marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package which-key
  :after (vertico)
  :ensure t
  :config
  (setq which-key-show-early-on-C-h t
      which-key-idle-delay 1e6 ; 11 days
      which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(eval-and-compile
  ;; Make leader macro available at compile- and load-time so downstream
  ;; use-package forms can macroexpand it.
  (require 'general)
  (general-create-definer dysthesis/start/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package general
  :ensure t
  :after (evil)
  :demand t
  :config
  (general-evil-setup)

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
  :after (:and embark consult)
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
:ensure t
:hook ((prog-mode text-mode markdown-mode) . smartparens-mode)
:config
(require 'smartparens-config))

(use-package treesit
  :defer t
  :preface
  (setq treesit-enabled-modes t)
  (setq treesit-font-lock-level 4)
  ;; HACK: The *-ts-mode major modes are inconsistent about how they treat
  ;;   missing language grammars (some error out, some respect
  ;;   `treesit-auto-install-grammar', some fall back to `fundamental-mode').
  ;;   I'd like to address this poor UX using `major-mode-remap-alist' entries
  ;;   created by `set-tree-sitter!' (which will fall back to the non-treesit
  ;;   modes), but most *-ts-mode's clobber `auto-mode-alist' and/or
  ;;   `interpreter-mode-alist' each time the major mode is activated, so those
  ;;   must be undone too so they don't overwrite user config.
  ;; TODO: Handle this during the 'doom sync' process instead.
  (save-match-data
    (dolist (sym '(auto-mode-alist interpreter-mode-alist))
      (set
       sym (cl-loop for (src . fn) in (symbol-value sym)
                    unless (and (functionp fn)
                                (string-match "-ts-mode\\(?:-maybe\\)?$" (symbol-name fn)))
                    collect (cons src fn)))))
  :config
  (dolist (map '((awk "https://github.com/Beaglefoot/tree-sitter-awk" nil nil nil nil)
                 (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex" nil nil nil nil)
                 (blueprint "https://github.com/huanie/tree-sitter-blueprint" nil nil nil nil)
                 (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp" nil nil nil nil)
                 (latex "https://github.com/latex-lsp/tree-sitter-latex" nil nil nil nil)
                 (make "https://github.com/tree-sitter-grammars/tree-sitter-make" nil nil nil nil)
                 (nu "https://github.com/nushell/tree-sitter-nu" nil nil nil nil)
                 (org "https://github.com/milisims/tree-sitter-org" nil nil nil nil)
                 (perl "https://github.com/ganezdragon/tree-sitter-perl" nil nil nil nil)
                 (proto "https://github.com/mitchellh/tree-sitter-proto" nil nil nil nil)
                 (r "https://github.com/r-lib/tree-sitter-r" nil nil nil nil)
                 (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages" nil nil nil)
                 (surface "https://github.com/connorlay/tree-sitter-surface" nil nil nil nil)
                 (toml "https://github.com/tree-sitter/tree-sitter-toml" nil nil nil nil)
                 (typst "https://github.com/uben0/tree-sitter-typst" "master" "src" nil nil)
                 (verilog "https://github.com/gmlarumbe/tree-sitter-verilog" nil nil nil nil)
                 (vhdl "https://github.com/alemuller/tree-sitter-vhdl" nil nil nil nil)
                 (vue "https://github.com/tree-sitter-grammars/tree-sitter-vue" nil nil nil nil)
                 (wast "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src" nil nil)
                 (wat "https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src" nil nil)
                 (wgsl "https://github.com/mehmetoguzderin/tree-sitter-wgsl" nil nil nil nil)))
    (cl-pushnew map treesit-language-source-alist :test #'eq :key #'car)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package evil-textobj-tree-sitter
  :ensure t
  :after (:and treesit evil)
  :config
  (defvar dysthesis/tree-sitter-inner-text-objects-map (make-sparse-keymap))
  (defvar dysthesis/tree-sitter-outer-text-objects-map (make-sparse-keymap))
  (defvar dysthesis/tree-sitter-goto-previous-map (make-sparse-keymap))
  (defvar dysthesis/tree-sitter-goto-next-map (make-sparse-keymap))

  (evil-define-key '(visual operator) 'tree-sitter-mode
    "i" dysthesis/tree-sitter-inner-text-objects-map
    "a" dysthesis/tree-sitter-outer-text-objects-map)
  (evil-define-key 'normal 'tree-sitter-mode
    "[g" dysthesis/tree-sitter-goto-previous-map
    "]g" dysthesis/tree-sitter-goto-next-map)

  ;; Inner text objects
  (define-key dysthesis/tree-sitter-inner-text-objects-map "A"
    (evil-textobj-tree-sitter-get-textobj ("parameter.inner" "call.inner")))
  (define-key dysthesis/tree-sitter-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key dysthesis/tree-sitter-inner-text-objects-map "F"
    (evil-textobj-tree-sitter-get-textobj "call.inner"))
  (define-key dysthesis/tree-sitter-inner-text-objects-map "C"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key dysthesis/tree-sitter-inner-text-objects-map "v"
    (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
  (define-key dysthesis/tree-sitter-inner-text-objects-map "l"
    (evil-textobj-tree-sitter-get-textobj "loop.inner"))

  ;; Outer text objects
  (define-key dysthesis/tree-sitter-outer-text-objects-map "A"
    (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.outer")))
  (define-key dysthesis/tree-sitter-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key dysthesis/tree-sitter-outer-text-objects-map "F"
    (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key dysthesis/tree-sitter-outer-text-objects-map "C"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key dysthesis/tree-sitter-outer-text-objects-map "c"
    (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key dysthesis/tree-sitter-outer-text-objects-map "v"
    (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key dysthesis/tree-sitter-outer-text-objects-map "l"
    (evil-textobj-tree-sitter-get-textobj "loop.outer"))

  ;; Goto previous
  (define-key dysthesis/tree-sitter-goto-previous-map "a"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t)))
  (define-key dysthesis/tree-sitter-goto-previous-map "f"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  (define-key dysthesis/tree-sitter-goto-previous-map "F"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "call.outer" t)))
  (define-key dysthesis/tree-sitter-goto-previous-map "C"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key dysthesis/tree-sitter-goto-previous-map "c"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "comment.outer" t)))
  (define-key dysthesis/tree-sitter-goto-previous-map "v"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t)))
  (define-key dysthesis/tree-sitter-goto-previous-map "l"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "loop.outer" t)))

  ;; Goto next
  (define-key dysthesis/tree-sitter-goto-next-map "a"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer")))
  (define-key dysthesis/tree-sitter-goto-next-map "f"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  (define-key dysthesis/tree-sitter-goto-next-map "F"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "call.outer")))
  (define-key dysthesis/tree-sitter-goto-next-map "C"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key dysthesis/tree-sitter-goto-next-map "c"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "comment.outer")))
  (define-key dysthesis/tree-sitter-goto-next-map "v"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "conditional.outer")))
  (define-key dysthesis/tree-sitter-goto-next-map "l"
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "loop.outer"))))

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimisation-init-p nil)

(define-minor-mode +lsp-optimisation-mode
  "Deploys universal GC and IPC optimisations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not (require 'gcmh nil 'noerror))
      (progn
        (setq +lsp-optimisation-mode nil)
        (message "[+lsp] gcmh not available; skipping GC optimisation"))
    (if (not +lsp-optimisation-mode)
        (setq-default read-process-output-max +lsp--default-read-process-output-max
                      gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                      +lsp--optimisation-init-p nil)
      ;; Only apply these settings once!
      (unless +lsp--optimisation-init-p
        (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
              +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
        (setq-default read-process-output-max (* 1024 1024))
        ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
        ;;        library, so we up the GC threshold to stave off GC-induced
        ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
        ;;        so we modify its variables rather than `gc-cons-threshold'
        ;;        directly.
        (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
        (when (fboundp 'gcmh-set-high-threshold)
          (gcmh-set-high-threshold))
        (setq +lsp--optimisation-init-p t)))))

(use-package eglot
  :defer t
  :hook ((prog-mode . (lambda ()
			 (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
			   (eglot-ensure))))
	    (eglot-managed-mode . +lsp-optimisation-mode))
  :custom
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
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

(when (executable-find "emacs-lsp-booster")
  (use-package eglot-booster
      :ensure nil
      :after eglot
      :config (eglot-booster-mode)))

(use-package consult-eglot
  :ensure t
  :after (:and eglot consult)
  :config
  (dysthesis/start/leader-keys
    "c s" '(consult-eglot-symbols :wk "Code Symbols")))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package dape
  :ensure t
  :preface
  (defconst dysthesis/dape-breakpoint-file
    (expand-file-name "dape/breakpoints" dysthesis/state-dir))
  (make-directory (file-name-directory dysthesis/dape-breakpoint-file) t)
  (defun dysthesis/dape--codelldb-dir-default ()
    "Compute the codelldb adapter directory from the environment."
    (let ((dir (getenv "CODELLDB_DIR")))
      (if (and dir (not (string= dir "")))
          dir
        (expand-file-name "debug-adapters" user-emacs-directory))))

  (defcustom dysthesis/dape-codelldb-dir (dysthesis/dape--codelldb-dir-default)
    "Directory containing the codelldb debug adapter."
    :type 'directory)

  (defun dysthesis/dape--codelldb-command ()
    "Return the codelldb adapter command path for `dape-configs`."
    (let* ((base (expand-file-name dysthesis/dape-codelldb-dir))
           (candidates (list
                        ;; Nix: CODELLDB_DIR is the adapter dir
                        (file-name-concat base "codelldb")
                        ;; Nix: CODELLDB_DIR is the extension root
                        (file-name-concat base "adapter" "codelldb")
                        ;; Default dape layout under debug-adapters
                        (file-name-concat base
                                          "codelldb"
                                          "extension"
                                          "adapter"
                                          "codelldb"))))
      (let ((found nil))
        (dolist (path candidates)
          (when (and (not found) (file-executable-p path))
            (setq found path)))
        (or found (car (last candidates))))))

  (defun dysthesis/dape--refresh-codelldb-configs ()
    "Refresh codelldb entries in `dape-configs`."
    (when (boundp 'dape-configs)
      (dolist (name '(codelldb-cc codelldb-rust))
        (let ((cfg (alist-get name dape-configs)))
          (when cfg
            (setf (alist-get name dape-configs)
                  (plist-put (copy-tree cfg)
                             'command
                             #'dysthesis/dape--codelldb-command)))))))

  (defun dysthesis/dape-refresh-adapter-dir (&rest _)
    "Refresh codelldb adapter settings from the current environment."
    (setq dysthesis/dape-codelldb-dir (dysthesis/dape--codelldb-dir-default))
    (dysthesis/dape--refresh-codelldb-configs))
  (dysthesis/dape-refresh-adapter-dir)
  (with-eval-after-load 'dape
    (dysthesis/dape-refresh-adapter-dir))
  ;; Keep `dape-adapter-dir` in sync when direnv updates environment vars.
  (with-eval-after-load 'direnv
    (advice-add 'direnv-update-directory-environment :after
                #'dysthesis/dape-refresh-adapter-dir))
  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :config
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer)

  ;; Persist breakpoints to state dir instead of store/home.
  (setq dape-breakpoint-file dysthesis/dape-breakpoint-file))

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :ensure t
  :custom
  (repeat-mode +1))

;; Left and right side windows occupy full frame height
(use-package emacs
  :custom
  (window-sides-vertical t))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :after (transient)
  :config
  (dysthesis/start/leader-keys
    "g g" '(magit :wk "Magit")))

(use-package majutsu
  :ensure nil
  :config
  (dysthesis/start/leader-keys
  "g j" '(majutsu :wk "Majutsu")))

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(use-package solaire-mode
 :ensure t
 :config
 (solaire-global-mode +1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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
  ((org-mode . mixed-pitch-mode)
   (markdown-mode . mixed-pitch-mode))
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

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package ligature
  :ensure t
  :hook (prog-mode . ligature-mode)
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

(use-package nael
  :ensure t
  :defer t
  :after eglot
  :hook
  ((nael-mode . eglot-ensure)
   (nael-mode . abbrev-mode)))

(use-package rustic
  :ensure t
  :after (corfu eglot treesit)
  ;; :bind (:map rustic-mode-map
  ;;             ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'dysthesisrustic-mode-hook))

(defun dysthesis/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "JBMono Nerd Font" :height 130))))
 '(variable-pitch ((t (:family "Atkinson Hyperlegible Next" :height 130)))))
