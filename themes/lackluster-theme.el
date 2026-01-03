;;; lackluster-theme.el --- Minimal lackluster theme (pure black / white) -*- lexical-binding: t; -*-

;;; Commentary:
;; Reimagines the Neovim lackluster palette for Emacs using the principles from
;; https://tonsky.me/blog/syntax-highlighting/ — very few colors, strong contrast,
;; and backgrounds for emphasis instead of a rainbow of hues.

;;; Code:

(defgroup lackluster-theme nil
  "Lackluster theme options."
  :group 'faces)

(defcustom lackluster-theme-bold-comments t
  "Bolden comments to make good commentary pop."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-bold-names t
  "Bolden names (functions, variables, types)."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-highlight-names nil
  "Deprecated: kept for compatibility. Name backgrounds are no longer used."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-use-background-accents t
  "Use subtle background blocks for selection, search, and definitions."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-highlight-comments t
  "Give comments a tinted background block."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-highlight-strings nil
  "Deprecated toggle; strings keep only their foreground color (no background)."
  :type 'boolean
  :group 'lackluster-theme)

(defcustom lackluster-theme-highlight-docstrings t
  "Highlight docstrings (where modes expose them separately)."
  :type 'boolean
  :group 'lackluster-theme)

(deftheme lackluster
  "Minimal, high-contrast Lackluster theme.")

;; Palette pulled from lackluster.nvim (lua/lackluster/color*.lua), remapped onto
;; a pure black canvas with pure white base text.
(let* ((lack "#708090")
       (luster "#deeeed")
       (orange "#ffaa88")
       (yellow "#abab77")
       (green "#789978")
       (blue "#7788AA")
       (red "#d70000")
       (gray0 "#000000")
       (gray1 "#080808")
       (gray2 "#191919")
       (gray3 "#2a2a2a")
       (gray4 "#444444")
       (gray5 "#555555")
       (gray6 "#7a7a7a")
       (gray7 "#aaaaaa")
       (gray8 "#cccccc")
       (gray9 "#dddddd")

       ;; Base surfaces
       (fg-main "#ffffff")
       (bg-main gray0)
       (bg-subtle gray1)
       (bg-muted gray2)
       (bg-panel gray3)
       (bg-strong gray4)

       ;; Semantic cues (keep them few and memorable)
       (fg-comment yellow)    ; highlight comments, don't hide them
       (fg-string green)      ; literals
       (fg-const blue)        ; constants / numbers
       (fg-def luster)        ; definitions / headings
       (fg-punct gray6)       ; punctuation muted

       ;; Tinted backgrounds for optional highlighting
       (bg-comment-hi "#3a340f")
       (bg-string-hi "#173824"))

  (custom-theme-set-variables
   'lackluster
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'lackluster

   ;; Core
   `(default ((t (:foreground ,fg-main :background ,bg-main))))
   `(cursor ((t (:background ,luster))))
   `(fringe ((t (:background ,bg-subtle :foreground ,gray5))))
   `(vertical-border ((t (:foreground ,gray4))))
   `(shadow ((t (:foreground ,gray6))))

   ;; Selection / focus
   `(region ((t ,(append (list :background (if lackluster-theme-use-background-accents bg-panel bg-main))
                         (when lackluster-theme-use-background-accents
                           '(:extend t))))))
   `(highlight ((t (:background ,bg-subtle))))
   `(hl-line ((t (:background ,bg-subtle :extend t))))

   ;; Search uses background so colors stay readable on pure black
   `(isearch ((t (:foreground ,gray0 :background ,yellow :weight bold))))
   `(lazy-highlight ((t (:foreground ,gray0 :background ,orange))))
   `(match ((t (:foreground ,gray0 :background ,blue))))

   ;; Deliberate punctuation and parens
   `(show-paren-match ((t (:background ,bg-strong :foreground ,fg-main :weight bold))))
   `(show-paren-mismatch ((t (:background ,red :foreground ,gray0 :weight bold))))

   ;; Line numbers
   `(line-number ((t (:foreground ,gray5 :background ,bg-subtle))))
   `(line-number-current-line ((t (:foreground ,luster :background ,bg-subtle :weight bold))))

   ;; Modeline
   `(mode-line ((t (:background ,bg-panel :foreground ,fg-main
                                :box (:line-width -1 :color ,gray4)))))
   `(mode-line-inactive ((t (:background ,bg-subtle :foreground ,gray6
                                         :box (:line-width -1 :color ,gray3)))))
   `(mode-line-buffer-id ((t (:weight bold :foreground ,fg-main))))

   ;; Prompts / links
   `(minibuffer-prompt ((t (:foreground ,luster :weight bold))))
   `(link ((t (:foreground ,blue :underline t))))
   `(link-visited ((t (:foreground ,lack :underline t))))

   ;; Syntax (minimal set)
   `(font-lock-comment-face
     ((t ,(let ((atts (list :foreground fg-comment)))
            (when lackluster-theme-bold-comments
              (setq atts (append atts '(:weight bold))))
            (when lackluster-theme-highlight-comments
              (setq atts (append atts (list :background bg-comment-hi))))
            atts))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-string-face ((t (:foreground ,fg-string))))
   `(font-lock-doc-face
     ((t ,(let ((atts (list :foreground fg-comment)))
            (when lackluster-theme-bold-comments
              (setq atts (append atts '(:weight bold))))
            (when lackluster-theme-highlight-docstrings
              (setq atts (append atts (list :background bg-comment-hi))))
            atts))))
   `(font-lock-constant-face ((t (:foreground ,fg-const))))
   `(font-lock-number-face ((t (:inherit font-lock-constant-face))))
   `(font-lock-builtin-face ((t (:foreground ,fg-punct))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg-punct))))
   `(font-lock-operator-face ((t (:foreground ,fg-punct))))
   `(font-lock-punctuation-face ((t (:foreground ,fg-punct))))
   `(font-lock-keyword-face ((t (:foreground ,fg-main))))          ; keywords not highlighted
   `(font-lock-variable-name-face
     ((t ,(let ((atts (list :foreground fg-main)))
            (when lackluster-theme-bold-names
              (setq atts (append atts '(:weight bold))))
            atts))))
   `(font-lock-function-name-face
     ((t ,(let ((atts (list :foreground fg-def)))
            (when lackluster-theme-bold-names
              (setq atts (append atts '(:weight bold))))
            atts))))
   `(font-lock-type-face ((t (:foreground ,fg-const))))           ; same hue as constants
   `(font-lock-warning-face ((t (:foreground ,red :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,fg-punct))))

   ;; UI / widgets
   `(tooltip ((t (:background ,bg-panel :foreground ,fg-main))))
   `(success ((t (:foreground ,green :weight bold))))
   `(warning ((t (:foreground ,orange :weight bold))))
   `(error ((t (:foreground ,red :weight bold))))
   `(compilation-info ((t (:foreground ,green))))
   `(compilation-warning ((t (:inherit warning))))
   `(compilation-error ((t (:inherit error))))

   ;; Completion (company / corfu / vertico)
   `(completions-common-part ((t (:foreground ,fg-def :weight bold))))
   `(completions-annotations ((t (:foreground ,gray6))))
   `(company-tooltip ((t (:foreground ,fg-main :background ,bg-panel))))
   `(company-tooltip-selection ((t (:foreground ,fg-main :background ,bg-muted :weight bold))))
   `(company-tooltip-common ((t (:foreground ,fg-def :weight bold))))
   `(company-scrollbar-bg ((t (:background ,bg-subtle))))
   `(company-scrollbar-fg ((t (:background ,bg-panel))))
   `(company-preview ((t (:background ,bg-subtle :foreground ,fg-main))))
   `(company-preview-common ((t (:foreground ,fg-def :weight bold))))
   `(corfu-default ((t (:foreground ,fg-main :background ,bg-panel))))
   `(corfu-current ((t (:foreground ,fg-main :background ,bg-muted :weight bold))))

   ;; Diff
   `(diff-added ((t (:foreground ,green :background ,(if lackluster-theme-use-background-accents gray1 bg-main)))))
   `(diff-removed ((t (:foreground ,red :background ,(if lackluster-theme-use-background-accents gray1 bg-main)))))
   `(diff-changed ((t (:foreground ,orange :background ,(if lackluster-theme-use-background-accents gray1 bg-main)))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))

   ;; Org-mode (minimal, readable)
   `(org-document-title ((t (:foreground ,fg-def :weight bold :height 1.2))))
   `(org-level-1 ((t (:foreground ,fg-def :weight bold :height 1.15))))
   `(org-level-2 ((t (:foreground ,fg-const :weight bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,fg-comment :weight bold))))
   `(org-code ((t (:foreground ,fg-string
                               :background ,(if lackluster-theme-use-background-accents bg-subtle bg-main)))))
   `(org-verbatim ((t (:foreground ,fg-const))))
   `(org-block ((t (:background ,(if lackluster-theme-use-background-accents bg-subtle bg-main) :extend t))))
   `(org-block-begin-line ((t (:foreground ,gray6 :background ,bg-subtle :extend t))))
   `(org-block-end-line ((t (:inherit org-block-begin-line))))
   `(org-todo ((t (:foreground ,orange :weight bold))))
   `(org-done ((t (:foreground ,green :weight bold))))

   ;; Whitespace
   `(whitespace-space ((t (:foreground ,gray3))))
   `(whitespace-tab ((t (:foreground ,gray3))))
   `(whitespace-trailing ((t (:background ,red :foreground ,gray0))))
   `(trailing-whitespace ((t (:inherit whitespace-trailing))))

   ;; VCS fringes
   `(git-gutter:added ((t (:foreground ,green))))
   `(git-gutter:deleted ((t (:foreground ,red))))
   `(git-gutter:modified ((t (:foreground ,orange))))

   ;; Term colors (keep ANSI consistent with palette)
   `(term-color-black ((t (:foreground ,gray4 :background ,gray4))))
   `(term-color-red ((t (:foreground ,red :background ,red))))
   `(term-color-green ((t (:foreground ,green :background ,green))))
   `(term-color-yellow ((t (:foreground ,yellow :background ,yellow))))
   `(term-color-blue ((t (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((t (:foreground ,lack :background ,lack))))
   `(term-color-cyan ((t (:foreground ,luster :background ,luster))))
   `(term-color-white ((t (:foreground ,fg-main :background ,fg-main))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lackluster)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; lackluster-theme.el ends here
