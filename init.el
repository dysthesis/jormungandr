;; Optimise the garbage collector to start up faster
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Get rid of WYSIWYG elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; Show relative line numbers on programming modes for easier Vim motions.
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Font configuration
(add-to-list 'default-frame-alist `(font . "JBMono Nerd Font 9"))

;; Use tree-sitter for
;;
;; - better syntax highlighting, and
;; - syntax-aware motions.
;;
;; TODO: Evil treesitter keybindings
(use-package tree-sitter
  :ensure t
  :hook
  (prog-mode . global-tree-sitter-mode))
(use-package tree-sitter-langs
  :ensure t)

(use-package general ;; Abstractions to neatly define keybindings
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

(use-package corfu ;; Automatic completions
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

(use-package nerd-icons-corfu ;; Iconise completion menu
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

(use-package tempel ;; Coding template engine
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

(use-package tempel-collection ;; A small collection of templates for tempel
  :ensure t
  :after tempel)

(use-package vertico ;; Better menu completion
  :ensure t
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package vertico-posframe ;; Centre vertico on the window
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

(use-package evil 
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-respect-visual-line-mode t) ;; respect visual lines

  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.

  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'

  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-want-keybinding nil)

  (setq evil-split-window-below t) ;; split windows created below
  (setq evil-vsplit-window-right t) ;; vertically split windows created to the right

  (setq evil-want-C-i-jump nil) ;; hopefully this will fix weird tab behaviour
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

(load-theme 'modus-vivendi t)

(use-package smartparens
  :ensure t
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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-persp-name t)
  (doom-modeline-persp-icon t))

(use-package transient
  :ensure t)
(use-package magit
  :ensure t
  :after transient
  :config
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "gg" '(magit :wk "Magit")))
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

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

(use-package org-modern ;; prettier org-mode display
  :demand t
  :config
  (global-org-modern-mode 1))

(use-package olivetti ;; centre org-mode text to make it more readable
  :ensure t
  :config
  (defun dysthesis/org-mode-setup ()
    (org-indent-mode)
    (olivetti-mode)
    (display-line-numbers-mode 0)
    (olivetti-set-width 90))
  (add-hook 'org-mode-hook 'dysthesis/org-mode-setup)) 

(use-package envrc ;; load per-project environments with direnv
  :ensure t
  :hook (prog-mode . envrc-mode))


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
