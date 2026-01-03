;;; doom-lackluster-theme.el --- A dark theme with little syntax highlighting -*- lexical-binding: t; no-byte-compile: t -*-
;; Copyright (C) 2025 Eduardo Apolinario
;;
;; Author: Eduardo Apolinario <curupa@gmail.com>
;; Maintainer: Eduardo Apolinario <curupa@gmail.com>
;; Version: 0.1
;; Keywords: themes faces
;; Homepage: https://github.com/eapolinario/doom-lackluster-theme
;; Homepage: https://github.com/agraul/doom-lackluster-theme
;; Homepage: https://github.com/tonsky/sublime-scheme-alabaster
;; Package-Requires: ((emacs "25.1") (doom-themes "2.3.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Doom Alabaster Dark is a port of tonsky's Alabaster theme based off of 
;; https://github.com/agraul/doom-lackluster-theme
;;
;;; Code:
(require 'doom-themes)

(defun doom-lackluster--setopt (sym val)
  "Custom setter that sets SYM to VAL and reloads the theme."
  (set-default sym val)
  ;;`custom--inhibit-theme-enable' is set to nil by `enable-theme'
  ;; don't reload when `enable-theme' is running
  (when (and (bound-and-true-p custom--inhibit-theme-enable)
             (memq 'doom-lackluster custom-enabled-themes))
    (load-theme 'doom-lackluster :no-confirm)))

(defgroup doom-lackluster-theme nil
  "Options for the `doom-lackluster' theme."
  :group 'doom-themes)

(defcustom doom-lackluster-comment-style 'faint
  "Control style of comments.

There are three styles to select from:
1. `original': very visible comment highlighting
2. `faint': a less vivid comment highlighting
3. nil: no comment highlighting."
  :group 'doom-lackluster-theme
  :type '(choice (const :tag "Original" original)
          (const :tag "faint" faint)
          (const :tag "nil" nil))
  :set #'doom-lackluster--setopt)

(def-doom-theme doom-lackluster
    "A dark theme with little highlighting"

;;;; Colors
  ;; name        default     256         16
  ((bg           '("#000000" "#000000"   "black"))
   (base0        '("#080808" "#080808"    "black"))
   (base1        '("#191919" "#191919"   "brightblack"))
   (base2        '("#2A2A2A" "#2A2A2A"   "brightblack"))
   (base3        '("#444444" "#444444"   "brightblack"))
   (base4        '("#555555" "#555555"   "brightblack"))
   (base5        '("#7A7A7A" "#7A7A7A"   "brightblack"))
   (base6        '("#AAAAAA" "#AAAAAA"   "brightblack"))
   (base7        '("#CCCCCC" "#CCCCCC"   "white"))
   (base8        '("#ECEEEE" "#ECEEEE"   "white"))
   (fg           '("#FFFFFF" "#FFFFFF"   "white"))
   (fg-alt       '("#DEEEED" "#DEEEED"   "brightblack"))
   (bg-alt       base0)

   (grey       base3)

   (red          '("#D70000" "#CC3333"   "red"))
   (green        '("#789978" "#789978"   "green"))
   (yellow       '("#FFAA88" "#FFAA88"   "yellow"))
   (dark-blue    '("#7788AA" "#7788AA"   "blue"))
   (magenta      '("#CC8BC9" "#CC8BC9"   "magenta"))
   (dark-cyan    '("#708090" "#708090"   "cyan"))
   (light-yellow '("#CD974B" "#CD974B"   "brightyellow"))
   (orange       '("#FFAA88" "#FFAA88"   "brightred"))
   (teal         '("#60CB00" "#60CB00"   "brightgreen"))
   (violet       '("#E64CE6" "#E64CE6"   "brightmagenta"))
   (cyan         '("#708090" "#708090"   "brightcyan"))
   (blue         '("#7788AA" "#7788AA"   "brightblue"))


   (yellow-highlight (doom-blend light-yellow bg 0.2))
   (bg-dark base2)
;;;; face categories -- required for all themes
   (highlight      light-yellow)
   (vertical-bar   base2)
   (selection      dark-blue)
   (builtin        fg)
   (comments       (cond
                    ((eq doom-lackluster-comment-style 'faint)
                     base4)
                    ((eq doom-lackluster-comment-style nil)
                     fg)
                    ;; 'original == default
                    (t "#DFDF8E")))
   (doc-comments   green)
   (constants      blue)
   (functions      dark-blue)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           dark-blue)
   (strings        green)
   (variables      fg)
   (numbers        fg-alt)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     yellow)

   (modeline-fg fg-alt)
   (modeline-bg bg-dark)
   (modeline-bg-inactive base0))

;;;; Base theme face overrides
  ((line-number                           :background bg-alt)
   (minibuffer-prompt                     :foreground dark-blue)
   (mode-line                             :foreground modeline-fg :background modeline-bg)
   (mode-line-emphasis                    :inherit 'mode-line :bold bold)
   (mode-line-inactive                    :foreground modeline-fg
                                          :background modeline-bg-inactive)
   (secondary-selection                   :background bg-dark)
   (hl-line                               :background (doom-blend "#FFFFFF" bg 0.06))
   ;; drop bold from link
   (link                                  :foreground highlight :underline t)
   (cursor                                :background light-yellow)
   ;;; Search highlight -> active
   (evil-ex-lazy-highlight                :foreground "#FFFFFF" :background "#2266AE")
   (evil-ex-search                        :foreground "#000000" :background light-yellow)
   (lazy-highlight                        :foreground "#000000" :background yellow-highlight)
   ;;;; vertico family
   (marginalia-size                       :foreground magenta)
   ;;;; all-the-icons
   (all-the-icons-purple                  :foreground magenta)
   ;;;; cider
   (cider-debug-code-overlay-face         :background base1)
   (cider-result-overlay-face             :background base1)
   ;;;; company
   (company-tooltip-annotation            :foreground fg)
   (company-tooltip-selection             :background bg-dark)
   ;;;; custom <built-in
   (custom-group-tag                      :foreground dark-blue)
   (custom-button-unraised                :foreground dark-blue :background bg
                                          :button '(line-width 1 :style nil))
   (custom-button-pressed-unraised        :foreground bg :background dark-blue
                                          :button '(line-width 1 :style nil))
   ;;;; diff-indicator
   (diff-added                            :inherit 'magit-diff-added-highlight)
   (diff-changed                          :inherit 'magit-diff-hunk-heading)
   (diff-hunk-header                      :inherit 'magit-diff-hunk-heading)
   (diff-removed                          :inherit 'magit-diff-removed-highlight)
   ;;;; dired <built-in>
   (dired-header                          :foreground dark-blue :bold bold)
   (dired-marked                          :foreground magenta :bold bold)
   (dired-broken-symlink                  :background warning :foreground bg-alt
                                          :bold bold)
   (dired-symlink                         :foreground dark-cyan)
   ;;;; dired-git-info
   (dgi-commit-message-face               :foreground fg-alt)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A                     :background (doom-blend selection bg 0.5)
                                          :bold bold :extend t)
   ;;;; eglot
   (eglot-highlight-symbol-face           :background bg-dark)
   ;;;; eshell <built-in>
   (eshell-prompt                         :foreground fg)
   (eshell-ls-archive                     :foreground fg)
   (eshell-ls-backup                      :foreground fg)
   (eshell-ls-clutter                     :foreground fg)
   (eshell-ls-directory                   :foreground dark-blue)
   (eshell-ls-executable                  :foreground fg)
   (eshell-ls-missing                     :foreground fg)
   (eshell-ls-product                     :foreground fg)
   (eshell-ls-readonly                    :foreground fg)
   (eshell-ls-special                     :foreground fg)
   (eshell-ls-symlink                     :inherit 'dired-symlink)
   (eshell-ls-unreadable                  :foreground fg)
   ;;;; font-lock-*-face <built-in>
   (font-lock-preprocessor-face           :foreground operators)
   (font-lock-preprocessor-char-face      :foreground operators)
   ;;;; forge
   (forge-topic-label                     :box '(:line-width -1))
   ;;;; git-commit
   (git-commit-comment-branch-local       :inherit 'magit-branch-local)
   (git-commit-comment-branch-remote      :inherit 'magit-branch-remote)
   (git-commit-comment-file               :foreground fg)
   (git-commit-keyword                    :foreground magenta :slant 'italic)
   ;;;; gnus
   (gnus-cite-1                           :foreground green)
   (gnus-cite-2                           :foreground dark-blue)
   (gnus-cite-3                           :foreground yellow)
   (gnus-cite-4                           :foreground blue)
   (gnus-cite-5                           :foreground green)
   (gnus-cite-6                           :foreground dark-blue)
   (gnus-cite-7                           :foreground yellow)
   (gnus-cite-8                           :foreground blue)
   (gnus-cite-9                           :foreground green)
   (gnus-cite-10                          :foreground dark-blue)
   (gnus-cite-11                          :foreground yellow)
   (gnus-signature                        :foreground fg-alt)
   ;;;; highlight-numbers-number
   (highlight-numbers-number              :foreground numbers)
   ;;;; lsp
   (lsp-face-highlight-textual            :background bg-dark)
   (lsp-flycheck-warning-deprecated-face  :inherit 'flycheck-warning)
   ;;;; lsp-rust
   (lsp-rust-analyzer-inlay-face          :foreground fg-alt :background bg-dark)
   ;;;; magit
   (magit-blame-heading                   :foreground dark-blue :background bg-dark)
   (magit-branch-local                    :foreground dark-blue)
   (magit-cherry-equivalent               :foreground magenta)
   (magit-cherry-unmatched                :foreground blue)
   (magit-diff-added                      :foreground green)
   (magit-diff-added-highlight            :foreground green
                                          :inherit 'magit-diff-context-highlight)
   (magit-diff-context                    :foreground base6 :background base1)
   (magit-diff-context-highlight          :foreground base7 :background base1)
   (magit-diff-hunk-heading               :foreground fg
                                          :background (doom-lighten base3 0.3))
   (magit-diff-hunk-heading-highlight     :foreground base7 :background bg-dark)
   (magit-diff-hunk-heading-highlight     :foreground bg-alt :background base3)
   (magit-diff-hunk-heading-selection     :foreground orange :background bg-dark)
   (magit-diff-our                        :background bg-alt)
   (magit-diff-our-highlight              :background bg-alt)
   (magit-diff-removed                    :foreground yellow)
   (magit-diff-removed-highlight          :foreground yellow
                                          :inherit 'magit-diff-context-highlight)
   (magit-dimmed                          :foreground fg-alt)
   (magit-filename                        :foreground fg)
   (magit-log-author                      :foreground fg)
   (magit-log-date                        :foreground green)
   (magit-log-graph                       :foreground magenta)
   (magit-hash                            :foreground magenta)
   (magit-section-secondary-heading       :foreground magenta :bold bold)
   (magit-tag                             :foreground blue)
   ;;;; markdown
   (markdown-code-face                    :background bg-alt)
   (markdown-bold-face                    :inherit 'bold :foreground fg)
   (markdown-italic-face                  :inherit 'italic :foreground fg)
   (markdown-inline-code-face             :foreground yellow :background bg-alt)
   (markdown-code-face                    :background bg-alt)
   (markdown-link-face                    :inherit 'link)
   (markdown-list-face                    :foreground fg)
   ;;;; message
   (message-header-name                   :foreground green)
   (message-header-other                  :foreground fg)
   (message-header-subject                :foreground fg)
   ;;; mmm
   (mmm-default-submode-face              :background bg-alt)
   ;;;; mu4e
   (mu4e-footer-face                      :foreground fg-alt)
   (mu4e-header-key-face                  :foreground fg)
   (mu4e-trashed-face                     :foreground fg-alt :strike-through t)
   ;;; nerd-icons
   (nerd-icons-purple                     :foreground magenta)
   ;;;; python
   (doom-lackluster-python-async-face :foreground dark-blue :weight 'bold)
   ;;;; org
   (org-agenda-date                       :foreground (doom-darken dark-blue 0.2))
   (org-agenda-date-today                 :foreground dark-blue)
   (org-agenda-date-weekend               :foreground (doom-darken dark-blue 0.3))
   (org-agenda-headline-done              :foreground fg-alt)
   (org-block                             :background bg-alt)
   (org-block-begin-line                  :background base1 :extend t)
   (org-code                              :foreground yellow :background bg-alt)
   (org-date                              :foreground fg-alt)
   (org-drawer                            :foreground fg-alt)
   (org-formula                           :foreground fg)
   (org-headline-done                     :foreground fg-alt)
   (org-level-1                           :inherit 'outline-1 :weight 'semibold)
   (org-property-value                    :foreground fg-alt)
   (org-special-keyword                   :foreground fg-alt)
   (org-table                             :background bg-alt)
   (org-tag                               :foreground fg)
   (org-verbatim                          :foreground green :background bg-alt)
   ;;;; outline (org-level-N faces inherit these)
   (outline-1                             :foreground fg)
   (outline-2                             :foreground dark-blue)
   (outline-3                             :foreground magenta)
   (outline-4                             :foreground blue)
   (outline-5                             :foreground (doom-darken yellow 0.2))
   (outline-6                             :foreground fg)
   (outline-7                             :foreground dark-blue)
   (outline-8                             :foreground magenta)
   ;;;; popup
   (popup-tip-face                        :inherit 'popup-face
                                          :foreground magenta :background bg-alt)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face       :foreground blue)
   (rainbow-delimiters-depth-2-face       :foreground green)
   (rainbow-delimiters-depth-3-face       :foreground yellow)
   (rainbow-delimiters-depth-4-face       :foreground blue)
   (rainbow-delimiters-depth-5-face       :foreground green)
   (rainbow-delimiters-depth-6-face       :foreground yellow)
   (rainbow-delimiters-depth-7-face       :foreground blue)
   (rainbow-delimiters-depth-8-face       :foreground green)
   (rainbow-delimiters-depth-9-face       :foreground yellow)
   ;;;; show-paren <built-in>
   (show-paren-match                      :foreground yellow :background bg-alt
                                          :weight 'ultra-bold :underline t)
   (show-paren-mismatch                   :foreground bg-alt :background yellow
                                          :weight 'ultra-bold :underline t)
   ;;;; smerge-tool
   (smerge-upper                          :background (doom-blend yellow bg 0.2))
   ;;;; tab-bar <built-in>
   (tab-bar                               :foreground fg :background modeline-bg)
   (tab-bar-tab                           :inherit 'tab-bar :underline t)
   (tab-bar-tab-inactive                  :inherit 'tab-bar)
   ;;;; treemacs
   (treemacs-git-modified-face            :foreground yellow)
   ;;;; tree-sitter
   (tree-sitter-hl-face:attribute         :foreground fg)
   (tree-sitter-hl-face:constant.builtin  :foreground magenta)
   (tree-sitter-hl-face:escape            :foreground base3)
   (tree-sitter-hl-face:function          :foreground dark-blue)
   (tree-sitter-hl-face:function.call     :foreground fg)
   (tree-sitter-hl-face:function.macro    :foreground fg)
   (tree-sitter-hl-face:function.special  :foreground fg)
   (tree-sitter-hl-face:label             :foreground fg)
   (tree-sitter-hl-face:string.special    :foreground strings)
   (tree-sitter-hl-face:property          :foreground fg)
   (tree-sitter-hl-face:punctuation       :foreground base3)
   (tree-sitter-hl-face:type.builtin      :foreground dark-blue)
   ;;;; vc and related faces
   (change-log-acknowledgment             :foreground magenta)
   (change-log-date                       :foreground green)
   (change-log-name                       :foreground fg)
   (change-log-list                       :foreground dark-blue) ; e.g. tags, branches
   (log-view-message                      :foreground magenta)
   (log-view-commit-body                  :foreground fg)
   ;;;; which-key
   (which-key-group-description-face      :foreground magenta)
   ;;;; widget <built-in>
   (widget-field                          :foreground fg :background bg-alt
                                          :box '(:line-width -1))
   ;;;; window-divider <built-in>
   (window-divider                        :foreground base3 :background base3)))

(defconst doom-lackluster--python-async-keywords
  '(("\\_<async\\_>" 0 'doom-lackluster-python-async-face prepend))
  "Font-lock keywords used to highlight `async' in Python buffers.")

(defvar python-mode-hook)
(defvar python-ts-mode-hook nil)

(defun doom-lackluster--python-buffer-setup ()
  "Activate Doom Alabaster Dark highlighting for `async`."
  (font-lock-remove-keywords nil doom-lackluster--python-async-keywords)
  (font-lock-add-keywords nil doom-lackluster--python-async-keywords 'append)
  (when font-lock-mode
    (font-lock-flush)
    (font-lock-ensure)))

(defun doom-lackluster--python-buffer-reset ()
  "Remove Doom Alabaster Dark highlighting for `async`."
  (font-lock-remove-keywords nil doom-lackluster--python-async-keywords)
  (when font-lock-mode
    (font-lock-flush)
    (font-lock-ensure)))

(defun doom-lackluster--python-iterate-buffers (fn)
  "Run FN in every live Python buffer."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'python-mode 'python-ts-mode)
          (funcall fn))))))

(defun doom-lackluster--python-theme-enable (theme)
  "Enable Python async highlighting when THEME is activated."
  (when (memq theme '(doom-lackluster doom-lackluster))
    (add-hook 'python-mode-hook #'doom-lackluster--python-buffer-setup)
    (when (boundp 'python-ts-mode-hook)
      (add-hook 'python-ts-mode-hook #'doom-lackluster--python-buffer-setup))
    (doom-lackluster--python-iterate-buffers
     #'doom-lackluster--python-buffer-setup)))

(defun doom-lackluster--python-theme-disable (theme)
  "Disable Python async highlighting when THEME is turned off."
  (when (memq theme '(doom-lackluster doom-lackluster))
    (remove-hook 'python-mode-hook #'doom-lackluster--python-buffer-setup)
    (when (boundp 'python-ts-mode-hook)
      (remove-hook 'python-ts-mode-hook #'doom-lackluster--python-buffer-setup))
    (doom-lackluster--python-iterate-buffers
     #'doom-lackluster--python-buffer-reset)))

(add-hook 'enable-theme-functions #'doom-lackluster--python-theme-enable)
(add-hook 'disable-theme-functions #'doom-lackluster--python-theme-disable)

(when (memq 'doom-lackluster custom-enabled-themes)
  (doom-lackluster--python-theme-enable 'doom-lackluster))
(when (and (memq 'doom-lackluster custom-enabled-themes)
           (not (memq 'doom-lackluster custom-enabled-themes)))
  (doom-lackluster--python-theme-enable 'doom-lackluster))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path (file-name-directory load-file-name)))

(provide 'doom-lackluster-theme)
;;; doom-lackluster-theme.el ends here
