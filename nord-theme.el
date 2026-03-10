;;; nord-theme.el --- An arctic, north-bluish clean and elegant theme

;; Copyright (c) 2016-2026 Sven Greb <development@svengreb.de>
;; Copyright (c) 2026-present Mark Willson

;; Title: Nord Theme
;; Project: nord-emacs
;; Version: 0.1.0
;; Author: Mark Willson
;; Package-Requires: ((emacs "27"))
;; License: MIT

;;; Commentary:

;; Nord is a 16 colorspace theme build to run in GUI- and terminal
;; mode with support for many third-party syntax- and UI packages.

;;; References:
;; GNU Emacs
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Face-Customization.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html

;;; Notes:

;;   This is a modified version of 0.6.0 nord-emacs (from
;;   https://github.com/nordtheme/emacs). The repository seems
;;   inactive.

;;; Code:

(unless (>= emacs-major-version 27)
  (error "Nord theme requires Emacs 27 or later!"))

(deftheme nord "An arctic, north-bluish clean and elegant theme")

(defgroup nord nil
  "Nord theme customizations.
  The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom nord-region-highlight nil
  "Allows to set a region highlight style based on the Nord components.
  Valid styles are
    - 'snowstorm' - Uses 'nord0' as foreground- and 'nord4' as background color
    - 'frost' - Uses 'nord0' as foreground- and 'nord8' as background color"
  :type 'string
  :group 'nord)

(defcustom nord-uniform-mode-lines nil
  "Enables uniform activate- and inactive mode lines using 'nord3'
  as background."
  :type 'boolean
  :group 'nord)

(defun nord-mk-face (name &rest args)
  "Return a custom theme face setting, called name. args should be a series of
  keyword value pairs. For a keyword argument of :foreground or
  :background, the value is expected to be a nord color constant. Other
  keyword value pairs are used unchanged as face specs."
  (if (or (memq :foreground args) (memq :background args))
      (let* ((clauses (vector '(default)
                              '(((class color) (min-colors 16777216)))
                              '(((class color) (min-colors 256)))
                              '(t))))
        (while args
          (let ((attr (pop args))
                (val (pop args)))
            (pcase attr
              ((or :foreground :background)
               (dotimes (c (1- (length clauses)))
                 (aset clauses (1+ c) (append (aref clauses (1+ c))
                                              (list attr (nth c val))))))
              (_ (aset clauses 0 (append (aref clauses 0) (list attr val)))))))
        (list name (mapcar #'identity clauses)))
    `(name ((t ,@args)))))

(defun nord-get-color (color)
  "Return color value from nord color list,
   based on current display capabilitity."
  (pcase (display-color-cells)
    (16777216 (car color))
    (256 (cadr color))
    (_ (caddr color))))

;;;; Color Constants
;;;  Each color is defined as a list of three display capabilities:
;;;  16M, 256 or other available colors.
(let* ((nord0 '("#2E3440" "#262626" "black"))
       (nord1 '("#3B4252" "#1C1C1C" "black"))
       (nord2 '("#434C5E" "#262626" "black"))
       (nord3 '("#4C566A" "#767676" "black"))
       (nord4 '("#D8DEE9" "#B2B2B2" "white"))
       (nord5 '("#E5E9F0" "#C6C6C6" "white"))
       (nord6 '("#ECEFF4" "#DADADA" "white"))
       (nord7 '("#8FBCBB" "#5FAFAF" "cyan"))
       (nord8 '("#88C0D0" "#5FAFFF""cyan"))
       (nord9 '("#81A1C1" "#5F87D7" "blue"))
       (nord10 '("#5E81AC" "#87AFFF" "blue"))
       (nord11 '("#BF616A" "#FF5FAF" "red"))
       (nord12 '("#D08770" "#AF5F5F" "yellow"))
       (nord13 '("#EBCB8B" "#D7AF00" "yellow"))
       (nord14 '("#A3BE8C" "#87AF5F" "green"))
       (nord15 '("#B48EAD" "#AF5FFF" "magenta"))
       (nord-annotation nord12)
       (nord-attribute nord7)
       (nord-class nord7)
       (nord-comment '("#7B88A1" "#767676" "blue")) ; brightened (was nord10)
       (nord-escape nord12)
       (nord-method nord8)
       (nord-keyword nord9)
       (nord-numeric nord15)
       (nord-operator nord9)
       (nord-preprocessor nord10)
       (nord-punctuation nord4)
       (nord-regexp nord13)
       (nord-string nord14)
       (nord-tag nord9)
       (nord-variable nord4)
       (nord-region-highlight-foreground
        (if (or (string= nord-region-highlight "frost")
                (string= nord-region-highlight "snowstorm"))
            nord0
          nord1))
       (nord-region-highlight-background
        (if (string= nord-region-highlight "frost")
            nord8
          nord4))
       (nord-uniform-mode-lines-background
        (if nord-uniform-mode-lines
            nord3
          nord1)))

;;;; +------------+
;;;; + Core Faces +
;;;; +------------+
  (custom-theme-set-faces
   'nord
   ;; +--- Base ---+
   (nord-mk-face 'bold :weight 'bold)
   (nord-mk-face 'bold-italic :weight 'bold :slant 'italic)
   (nord-mk-face 'italic :slant 'italic)
   (nord-mk-face 'underline :underline t)
   (nord-mk-face 'shadow :foreground nord3)
   (nord-mk-face 'warning :foreground nord13 :weight 'bold)
   (nord-mk-face 'default :foreground nord4 :background nord0)
   (nord-mk-face 'error :foreground nord11 :weight 'bold)
   (nord-mk-face 'escape-glyph :foreground nord12)
   ;; +--- Syntax ---+
   (nord-mk-face 'font-lock-builtin-face :foreground nord9)
   (nord-mk-face 'font-lock-comment-face :foreground nord-comment)
   (nord-mk-face 'font-lock-comment-delimiter-face :foreground nord-comment)
   (nord-mk-face 'font-lock-constant-face :foreground nord9)
   (nord-mk-face 'font-lock-doc-face :foreground nord-comment)
   (nord-mk-face 'font-lock-function-name-face :foreground nord8)
   (nord-mk-face 'font-lock-keyword-face :foreground nord9)
   (nord-mk-face 'font-lock-negation-char-face :foreground nord9)
   (nord-mk-face 'font-lock-preprocessor-face :foreground nord10 :weight 'bold)
   (nord-mk-face 'font-lock-reference-face :foreground nord9)
   (nord-mk-face 'font-lock-regexp-grouping-backslash :foreground nord13)
   (nord-mk-face 'font-lock-regexp-grouping-construct :foreground nord13)
   (nord-mk-face 'font-lock-string-face :foreground nord14)
   (nord-mk-face 'font-lock-type-face :foreground nord7)
   (nord-mk-face 'font-lock-variable-name-face :foreground nord4)
   ;; > C
   (nord-mk-face 'c-annotation-face :foreground nord-annotation)

   ;; > diff
   (nord-mk-face 'diff-added :foreground nord14)
   (nord-mk-face 'diff-changed :foreground nord13)
   (nord-mk-face 'diff-context :inherit 'default)
   (nord-mk-face 'diff-file-header :foreground nord8)
   (nord-mk-face 'diff-function :foreground nord7)
   (nord-mk-face 'diff-header :foreground nord9 :weight 'bold)
   (nord-mk-face 'diff-hunk-header :foreground nord9 :background nord0)
   (nord-mk-face 'diff-indicator-added :foreground nord14)
   (nord-mk-face 'diff-indicator-changed :foreground nord13)
   (nord-mk-face 'diff-indicator-removed :foreground nord11)
   (nord-mk-face 'diff-nonexistent :foreground nord11)
   (nord-mk-face 'diff-refine-added :foreground nord14)
   (nord-mk-face 'diff-refine-changed :foreground nord13)
   (nord-mk-face 'diff-refine-removed :foreground nord11)
   (nord-mk-face 'diff-removed :foreground nord11)

   ;; +--- UI ---+
   (nord-mk-face 'border :foreground nord4)
   (nord-mk-face 'buffer-menu-buffer :foreground nord4 :weight 'bold)
   (nord-mk-face 'button :background nord0 :foreground nord8
                 :box `(:line-width 2 :color ,(nord-get-color nord4)
                                    :style pressed-button))
   (nord-mk-face 'completions-annotations :foreground nord9)
   (nord-mk-face 'completions-common-part :foreground nord8 :weight 'bold)
   (nord-mk-face 'completions-first-difference :foreground nord11)
   (nord-mk-face 'custom-button :background nord0 :foreground nord8
                 :box `(:line-width 2 :color ,(nord-get-color nord4)
                                    :style pressed-button))
   (nord-mk-face 'custom-button-mouse :background nord4 :foreground nord0
                 :box `(:line-width 2 :color ,(nord-get-color nord4)
                                    :style pressed-button))
   (nord-mk-face 'custom-button-pressed :background nord6 :foreground nord0
                 :box `(:line-width 2 :color ,(nord-get-color nord4)
                                    :style pressed-button))
   (nord-mk-face 'custom-button-pressed-unraised :background nord4
                 :foreground nord0
                 :box `(:line-width 2 :color ,(nord-get-color nord4)
                                    :style pressed-button))
   (nord-mk-face 'custom-button-unraised :background nord0 :foreground nord8
                 :box `(:line-width 2 :color ,(nord-get-color nord4)
                                    :style pressed-button))
   (nord-mk-face 'custom-changed :foreground nord13)
   (nord-mk-face 'custom-comment :foreground nord-comment)
   (nord-mk-face 'custom-comment-tag :foreground nord7)
   (nord-mk-face 'custom-documentation :foreground nord4)
   (nord-mk-face 'custom-group-tag :foreground nord8 :weight 'bold)
   (nord-mk-face 'custom-group-tag-1 :foreground nord8 :weight 'bold)
   (nord-mk-face 'custom-invalid :foreground nord11)
   (nord-mk-face 'custom-modified :foreground nord13)
   (nord-mk-face 'custom-rogue :foreground nord12 :background nord2)
   (nord-mk-face 'custom-saved :foreground nord14)
   (nord-mk-face 'custom-set :foreground nord8)
   (nord-mk-face 'custom-state :foreground nord14)
   (nord-mk-face 'custom-themed :foreground nord8 :background nord2)
   (nord-mk-face 'cursor :background nord4)
   (nord-mk-face 'fringe :foreground nord4 :background nord0)
   (nord-mk-face 'file-name-shadow :inherit 'shadow)
   (nord-mk-face 'header-line :foreground nord4 :background nord2)
   (nord-mk-face 'help-argument-name :foreground nord8)
   (nord-mk-face 'highlight :foreground nord8 :background nord2)
   (nord-mk-face 'hl-line :background nord1)
   (nord-mk-face 'info-menu-star :foreground nord9)
   (nord-mk-face 'isearch :foreground nord0 :background nord8)
   (nord-mk-face 'isearch-fail :foreground nord11)
   (nord-mk-face 'link :underline t)
   (nord-mk-face 'link-visited :underline t)
   (nord-mk-face 'linum :foreground nord3 :background nord0)
   (nord-mk-face 'linum-relative-current-face :foreground nord3
                 :background nord0)
   (nord-mk-face 'match :inherit 'isearch)
   (nord-mk-face 'message-cited-text :foreground nord4)
   (nord-mk-face 'message-header-cc :foreground nord9)
   (nord-mk-face 'message-header-name :foreground nord7)
   (nord-mk-face 'message-header-newsgroup :foreground nord14)
   (nord-mk-face 'message-header-other :foreground nord4)
   (nord-mk-face 'message-header-subject :foreground nord8)
   (nord-mk-face 'message-header-to :foreground nord9)
   (nord-mk-face 'message-header-xheader :foreground nord13)
   (nord-mk-face 'message-mml :foreground nord10)
   (nord-mk-face 'message-separator :inherit 'shadow)
   (nord-mk-face 'minibuffer-prompt :foreground nord8 :weight 'bold)
   (nord-mk-face 'mm-command-output :foreground nord8)
   (nord-mk-face 'mode-line :foreground nord8 :background nord2)
   (nord-mk-face 'mode-line-buffer-id :weight 'bold)
   (nord-mk-face 'mode-line-highlight :inherit 'highlight)
   (nord-mk-face 'mode-line-inactive :foreground nord4
                 :background nord-uniform-mode-lines-background)
   (nord-mk-face 'next-error :inherit 'error)
   (nord-mk-face 'nobreak-space :foreground nord3)
   (nord-mk-face 'outline-1 :foreground nord8 :weight 'bold)
   (nord-mk-face 'outline-2 :inherit 'outline-1)
   (nord-mk-face 'outline-3 :inherit 'outline-1)
   (nord-mk-face 'outline-4 :inherit 'outline-1)
   (nord-mk-face 'outline-5 :inherit 'outline-1)
   (nord-mk-face 'outline-6 :inherit 'outline-1)
   (nord-mk-face 'outline-7 :inherit 'outline-1)
   (nord-mk-face 'outline-8 :inherit 'outline-1)
   (nord-mk-face 'package-description :foreground nord4)
   (nord-mk-face 'package-help-section-name :foreground nord8 :weight 'bold)
   (nord-mk-face 'package-name :foreground nord8)
   (nord-mk-face 'package-status-available :foreground nord7)
   (nord-mk-face 'package-status-avail-obso :foreground nord7 :slant 'italic)
   (nord-mk-face 'package-status-built-in :foreground nord9)
   (nord-mk-face 'package-status-dependency :foreground nord8 :slant 'italic)
   (nord-mk-face 'package-status-disabled :foreground nord3)
   (nord-mk-face 'package-status-external :foreground nord12 :slant 'italic)
   (nord-mk-face 'package-status-held :foreground nord4 :weight 'bold)
   (nord-mk-face 'package-status-new :foreground nord14)
   (nord-mk-face 'package-status-incompat :foreground nord11)
   (nord-mk-face 'package-status-installed :foreground nord7 :weight 'bold)
   (nord-mk-face 'package-status-unsigned :underline nord13)
   (nord-mk-face 'region :foreground nord-region-highlight-foreground
                 :background nord-region-highlight-background)
   (nord-mk-face 'scroll-bar :background nord3)
   (nord-mk-face 'secondary-selection :background nord2)
   (nord-mk-face 'show-paren-match-face :foreground nord0 :background nord8)
   (nord-mk-face 'show-paren-mismatch-face :background nord11)

   (nord-mk-face 'show-paren-match :foreground nord0 :background nord8)
   (nord-mk-face 'show-paren-mismatch :background nord11)
   (nord-mk-face 'success :foreground nord14)
   (nord-mk-face 'term :foreground nord4 :background nord0)
   (nord-mk-face 'term-color-black :foreground nord1 :background nord1)
   (nord-mk-face 'term-color-white :foreground nord5 :background nord5)
   (nord-mk-face 'term-color-cyan :foreground nord7 :background nord7)
   (nord-mk-face 'term-color-blue :foreground nord8 :background nord8)
   (nord-mk-face 'term-color-red :foreground nord11 :background nord11)
   (nord-mk-face 'term-color-yellow :foreground nord13 :background nord13)
   (nord-mk-face 'term-color-green :foreground nord14 :background nord14)
   (nord-mk-face 'term-color-magenta :foreground nord15 :background nord15)
   (nord-mk-face 'tool-bar :foreground nord4 :background nord3)
   (nord-mk-face 'tooltip :foreground nord0 :background nord4)
   (nord-mk-face 'trailing-whitespace :foreground nord3)
   (nord-mk-face 'tty-menu-disabled-face :foreground nord1)
   (nord-mk-face 'tty-menu-enabled-face :background nord2 :foreground nord4)
   (nord-mk-face 'tty-menu-selected-face :foreground nord8 :underline t)
   (nord-mk-face 'undo-tree-visualizer-current-face :foreground nord8)
   (nord-mk-face 'undo-tree-visualizer-default-face :foreground nord4)
   (nord-mk-face 'undo-tree-visualizer-unmodified-face :foreground nord4)
   (nord-mk-face 'undo-tree-visualizer-register-face :foreground nord9)
   (nord-mk-face 'vc-conflict-state :foreground nord12)
   (nord-mk-face 'vc-edited-state :foreground nord13)
   (nord-mk-face 'vc-locally-added-state :underline t)
   (nord-mk-face 'vc-locked-state :foreground nord10)
   (nord-mk-face 'vc-missing-state :foreground nord11)
   (nord-mk-face 'vc-needs-update-state :foreground nord12)
   (nord-mk-face 'vc-removed-state :foreground nord11)
   (nord-mk-face 'vc-state-base :foreground nord4)
   (nord-mk-face 'vc-up-to-date-state :foreground nord8)
   (nord-mk-face 'vertical-border :foreground nord2)
   (nord-mk-face 'which-func :foreground nord8)
   (nord-mk-face 'whitespace-big-indent :foreground nord3 :background nord0)
   (nord-mk-face 'whitespace-empty :foreground nord3 :background nord0)
   (nord-mk-face 'whitespace-hspace :foreground nord3 :background nord0)
   (nord-mk-face 'whitespace-indentation :foreground nord3 :background nord0)
   (nord-mk-face 'whitespace-line :background nord0)
   (nord-mk-face 'whitespace-newline :foreground nord3 :background nord0)
   (nord-mk-face 'whitespace-space :foreground nord3 :background nord0)
   (nord-mk-face 'whitespace-space-after-tab :foreground nord3
                 :background nord0)
   (nord-mk-face 'whitespace-space-before-tab :foreground nord3
                 :background nord0)
   (nord-mk-face 'whitespace-tab :foreground nord3 :background nord0)
   (nord-mk-face 'whitespace-trailing :inherit 'trailing-whitespace)
   (nord-mk-face 'widget-button-pressed :foreground nord9 :background nord1)
   (nord-mk-face 'widget-documentation :foreground nord4)
   (nord-mk-face 'widget-field :background nord2 :foreground nord4)
   (nord-mk-face 'widget-single-line-field :background nord2
                 :foreground nord4)
   (nord-mk-face 'window-divider :background nord3)
   (nord-mk-face 'window-divider-first-pixel :background nord3)
   (nord-mk-face 'window-divider-last-pixel :background nord3)

    ;;;; +-----------------+
    ;;;; + Package Support +
    ;;;; +-----------------+
   ;; +--- Syntax ---+
   ;; > Auctex
   (nord-mk-face 'font-latex-bold-face :inherit 'bold)
   (nord-mk-face 'font-latex-italic-face :inherit 'italic)
   (nord-mk-face 'font-latex-math-face :foreground nord8)
   (nord-mk-face 'font-latex-sectioning-0-face :foreground nord8
                 :weight 'bold)
   (nord-mk-face 'font-latex-sectioning-1-face
                 :inherit 'font-latex-sectioning-0-face)
   (nord-mk-face 'font-latex-sectioning-2-face
                 :inerit 'font-latex-sectioning-0-face)
   (nord-mk-face 'font-latex-sectioning-3-face
                 :inherit 'font-latex-sectioning-0-face)
   (nord-mk-face 'font-latex-sectioning-4-face
                 :inherit 'font-latex-sectioning-0-face)
   (nord-mk-face 'font-latex-sectioning-5-face
                 :inherit 'font-latex-sectioning-0-face)
   (nord-mk-face 'font-latex-script-char-face :inherit 'font-lock-warning-face)
   (nord-mk-face 'font-latex-string-face :inherit 'font-lock-string-face)
   (nord-mk-face 'font-latex-warning-face :inherit 'font-lock-warning-face)

   ;; > Elixir
   (nord-mk-face 'elixir-attribute-face :foreground nord-annotation)
   (nord-mk-face 'elixir-atom-face :foreground nord4 :weight 'bold)

   ;; > Enhanced Ruby
   (nord-mk-face 'enh-ruby-heredoc-delimiter-face :foreground nord14)
   (nord-mk-face 'enh-ruby-op-face :foreground nord9)
   (nord-mk-face 'enh-ruby-regexp-delimiter-face :foreground nord13)
   (nord-mk-face 'enh-ruby-regexp-face :foreground nord13)
   (nord-mk-face 'enh-ruby-string-delimiter-face :foreground nord14)
   (nord-mk-face 'erm-syn-errline :foreground nord11 :underline t)
   (nord-mk-face 'erm-syn-warnline :foreground nord13 :underline t)

   ;; > Java Development Environment for Emacs
   (nord-mk-face 'jdee-db-active-breakpoint-face :background nord2 :
                 :weight 'bold)
   (nord-mk-face 'jdee-bug-breakpoint-cursor :background nord2)
   (nord-mk-face 'jdee-db-requested-breakpoint-face :foreground nord13
                 :background nord2 :weight 'bold)
   (nord-mk-face 'jdee-db-spec-breakpoint-face :foreground nord14
                 :background nord2 :weight 'bold)
   (nord-mk-face 'jdee-font-lock-api-face :foreground nord4)
   (nord-mk-face 'jdee-font-lock-code-face :slant 'italic)
   (nord-mk-face 'jdee-font-lock-constant-face :foreground nord-keyword)
   (nord-mk-face 'jdee-font-lock-constructor-face :foreground nord-method)
   (nord-mk-face 'jdee-font-lock-doc-tag-face :foreground nord7)
   (nord-mk-face 'jdee-font-lock-link-face :underline t)
   (nord-mk-face 'jdee-font-lock-modifier-face :foreground nord-keyword)
   (nord-mk-face 'jdee-font-lock-number-face :foreground nord-numeric)
   (nord-mk-face 'jdee-font-lock-operator-fac :foreground nord-operator)
   (nord-mk-face 'jdee-font-lock-package-face :foreground nord-class)
   (nord-mk-face 'jdee-font-lock-pre-face :foreground nord-comment
                 :slant 'italic)
   (nord-mk-face 'jdee-font-lock-private-face :foreground nord-keyword)
   (nord-mk-face 'jdee-font-lock-public-face :foreground nord-keyword)
   (nord-mk-face 'jdee-font-lock-variable-face :foreground nord-variable)

   ;; > JavaScript 2
   (nord-mk-face 'js2-function-call :foreground nord8)
   (nord-mk-face 'js2-private-function-call :foreground nord8)
   (nord-mk-face 'js2-jsdoc-html-tag-delimiter :foreground nord6)
   (nord-mk-face 'js2-jsdoc-html-tag-name :foreground nord9)
   (nord-mk-face 'js2-external-variable :foreground nord4)
   (nord-mk-face 'js2-function-param :foreground nord4)
   (nord-mk-face 'js2-jsdoc-value :foreground nord-comment)
   (nord-mk-face 'js2-jsdoc-tag :foreground nord7)
   (nord-mk-face 'js2-jsdoc-type :foreground nord7)
   (nord-mk-face 'js2-private-member :foreground nord4)
   (nord-mk-face 'js2-object-property :foreground nord4)
   (nord-mk-face 'js2-error :foreground nord11)
   (nord-mk-face 'js2-warning :foreground nord13)
   (nord-mk-face 'js2-instance-member :foreground nord4)

   ;; > JavaScript 3
   (nord-mk-face 'js3-error-face :foreground nord11)
   (nord-mk-face 'js3-external-variable-face :foreground nord4)
   (nord-mk-face 'js3-function-param-face :foreground nord4)
   (nord-mk-face 'js3-instance-member-face :foreground nord4)
   (nord-mk-face 'js3-jsdoc-html-tag-delimiter-face :foreground nord6)
   (nord-mk-face 'js3-jsdoc-html-tag-name-face :foreground nord9)
   (nord-mk-face 'js3-jsdoc-tag-face :foreground nord9)
   (nord-mk-face 'js3-jsdoc-type-face :foreground nord7)
   (nord-mk-face 'js3-jsdoc-value-face :foreground nord4)
   (nord-mk-face 'js3-magic-paren-face :inherit 'show-paren-match-face)
   (nord-mk-face 'js3-private-function-call-face :foreground nord8)
   (nord-mk-face 'js3-private-member-face :foreground nord4)
   (nord-mk-face 'js3-warning-face :foreground nord13)

   ;; > Markdown
   (nord-mk-face 'markdown-blockquote-face :foreground nord-comment)
   (nord-mk-face 'markdown-'bold-face :inherit 'bold)
   (nord-mk-face 'markdown-header-face-1 :foreground nord8)
   (nord-mk-face 'markdown-header-face-2 :foreground nord8)
   (nord-mk-face 'markdown-header-face-3 :foreground nord8)
   (nord-mk-face 'markdown-header-face-4 :foreground nord8)
   (nord-mk-face 'markdown-header-face-5 :foreground nord8)
   (nord-mk-face 'markdown-header-face-6 :foreground nord8)
   (nord-mk-face 'markdown-inline-code-face :foreground nord7)
   (nord-mk-face 'markdown-italic-face :inherit 'italic)
   (nord-mk-face 'markdown-link-face :foreground nord8)
   (nord-mk-face 'markdown-markup-face :foreground nord9)
   (nord-mk-face 'markdown-reference-face :inherit 'markdown-link-face)
   (nord-mk-face 'markdown-url-face :foreground nord4 :underline t)

   ;; > Rainbow Delimeters
   (nord-mk-face 'rainbow-delimiters-depth-1-face :foreground nord7)
   (nord-mk-face 'rainbow-delimiters-depth-2-face :foreground nord8)
   (nord-mk-face 'rainbow-delimiters-depth-3-face :foreground nord9)
   (nord-mk-face 'rainbow-delimiters-depth-4-face :foreground nord10)
   (nord-mk-face 'rainbow-delimiters-depth-5-face :foreground nord12)
   (nord-mk-face 'rainbow-delimiters-depth-6-face :foreground nord13)
   (nord-mk-face 'rainbow-delimiters-depth-7-face :foreground nord14)
   (nord-mk-face 'rainbow-delimiters-depth-8-face :foreground nord15)
   (nord-mk-face 'rainbow-delimiters-unmatched-face :foreground nord11)

   ;; > Web Mode
   (nord-mk-face 'web-mode-attr-tag-custom-face :foreground nord-attribute)
   (nord-mk-face 'web-mode-builtin-face :foreground nord-keyword)
   (nord-mk-face 'web-mode-comment-face :foreground nord-comment)
   (nord-mk-face 'web-mode-comment-keyword-face :foreground nord-comment)
   (nord-mk-face 'web-mode-constant-face :foreground nord-variable)
   (nord-mk-face 'web-mode-css-at-rule-face :foreground nord-annotation)
   (nord-mk-face 'web-mode-css-function-face :foreground nord-method)
   (nord-mk-face 'web-mode-css-property-name-face :foreground nord-keyword)
   (nord-mk-face 'web-mode-css-pseudo-class-face :foreground nord-class)
   (nord-mk-face 'web-mode-css-selector-face :foreground nord-keyword)
   (nord-mk-face 'web-mode-css-string-face :foreground nord-string)
   (nord-mk-face 'web-mode-doctype-face :foreground nord-preprocessor)
   (nord-mk-face 'web-mode-function-call-face :foreground nord-method)
   (nord-mk-face 'web-mode-function-name-face :foreground nord-method)
   (nord-mk-face 'web-mode-html-attr-name-face :foreground nord-attribute)
   (nord-mk-face 'web-mode-html-attr-equal-face :foreground nord-punctuation)
   (nord-mk-face 'web-mode-html-attr-value-face :foreground nord-string)
   (nord-mk-face 'web-mode-html-entity-face :foreground nord-keyword)
   (nord-mk-face 'web-mode-html-tag-bracket-face :foreground nord-punctuation)
   (nord-mk-face 'web-mode-html-tag-custom-face :foreground nord-tag)
   (nord-mk-face 'web-mode-html-tag-face :foreground nord-tag)
   (nord-mk-face 'web-mode-html-tag-namespaced-face :foreground nord-keyword)
   (nord-mk-face 'web-mode-json-key-face :foreground nord-class)
   (nord-mk-face 'web-mode-json-string-face :foreground nord-string)
   (nord-mk-face 'web-mode-keyword-face :foreground nord-keyword)
   (nord-mk-face 'web-mode-preprocessor-face :foreground nord-preprocessor)
   (nord-mk-face 'web-mode-string-face :foreground nord-string)
   (nord-mk-face 'web-mode-symbol-face :foreground nord-variable)
   (nord-mk-face 'web-mode-type-face :foreground nord-class)
   (nord-mk-face 'web-mode-warning-face :inherit 'font-lock-warning-face)
   (nord-mk-face 'web-mode-variable-name-face :foreground nord-variable)

   ;; +--- UI ---+
   ;; > Anzu
   (nord-mk-face 'anzu-mode-line :foreground nord8)
   (nord-mk-face 'anzu-mode-line-no-match :foreground nord11)

   ;; > Avy
   (nord-mk-face 'avy-lead-face :background nord11 :foreground nord5)
   (nord-mk-face 'avy-lead-face-0 :background nord10 :foreground nord5)
   (nord-mk-face 'avy-lead-face-1 :background nord3 :foreground nord5)
   (nord-mk-face 'avy-lead-face-2 :background nord15 :foreground nord5)

   ;; > Company
   (nord-mk-face 'company-echo-common :foreground nord0 :background nord4)
   (nord-mk-face 'company-preview :foreground nord4 :background nord10)
   (nord-mk-face 'company-preview-common :foreground nord0 :background nord8)
   (nord-mk-face 'company-preview-search :foreground nord0 :background nord8)
   (nord-mk-face 'company-scrollbar-bg :foreground nord1 :background nord1)
   (nord-mk-face 'company-scrollbar-fg :foreground nord2 :background nord2)
   (nord-mk-face 'company-template-field :foreground nord0 :background nord7)
   (nord-mk-face 'company-tooltip :foreground nord4 :background nord2)
   (nord-mk-face 'company-tooltip-annotation :foreground nord12)
   (nord-mk-face 'company-tooltip-annotation-selection :foreground nord12
                 :weight 'bold)
   (nord-mk-face 'company-tooltip-common :foreground nord8)
   (nord-mk-face 'company-tooltip-common-selection :foreground nord8
                 :background nord3)
   (nord-mk-face 'company-tooltip-mouse :inherit 'highlight)
   (nord-mk-face 'company-tooltip-selection :background nord3 :weight 'bold)

   ;; > diff-hl
   (nord-mk-face 'diff-hl-change :background nord13)
   (nord-mk-face 'diff-hl-insert :background nord14)
   (nord-mk-face 'diff-hl-delete :background nord11)

   ;; > Evil
   (nord-mk-face 'evil-ex-info :foreground nord8)
   (nord-mk-face 'evil-ex-substitute-replacement :foreground nord9)
   (nord-mk-face 'evil-ex-substitute-matches :inherit 'isearch)

   ;; > Flycheck
   (nord-mk-face 'flycheck-error :underline
                 `(:style wave :color ,(nord-get-color nord11)))
   (nord-mk-face 'flycheck-fringe-error :foreground nord11 :weight 'bold)
   (nord-mk-face 'flycheck-fringe-info :foreground nord8 :weight 'bold)
   (nord-mk-face 'flycheck-fringe-warning :foreground nord13 :weight 'bold)
   (nord-mk-face 'flycheck-info
                 :underline `(:style wave :color ,(nord-get-color nord8)))
   (nord-mk-face 'flycheck-warning :underline
                 `(:style wave :color ,(nord-get-color nord13)))

   ;; > Git Gutter
   (nord-mk-face 'git-gutter:modified :foreground nord13)
   (nord-mk-face 'git-gutter:added :foreground nord14)
   (nord-mk-face 'git-gutter:deleted :foreground nord11)

   ;; > Git Gutter Plus
   (nord-mk-face 'git-gutter+-modified :foreground nord13)
   (nord-mk-face 'git-gutter+-added :foreground nord14)
   (nord-mk-face 'git-gutter+-deleted :foreground nord11)

   ;; > Helm
   (nord-mk-face 'helm-bookmark-addressbook :foreground nord7)
   (nord-mk-face 'helm-bookmark-directory :foreground nord9)
   (nord-mk-face 'helm-bookmark-file :foreground nord8)
   (nord-mk-face 'helm-bookmark-gnus :foreground nord10)
   (nord-mk-face 'helm-bookmark-info :foreground nord14)
   (nord-mk-face 'helm-bookmark-man :foreground nord4)
   (nord-mk-face 'helm-bookmark-w3m :foreground nord9)
   (nord-mk-face 'helm-buffer-directory :foreground nord9)
   (nord-mk-face 'helm-buffer-file :foreground nord8)
   (nord-mk-face 'helm-buffer-not-saved :foreground nord13)
   (nord-mk-face 'helm-buffer-process :foreground nord10)
   (nord-mk-face 'helm-candidate-number :foreground nord4 :weight 'bold)
   (nord-mk-face 'helm-candidate-number-suspended :foreground nord4)
   (nord-mk-face 'helm-ff-directory :foreground nord9 :weight 'bold)
   (nord-mk-face 'helm-ff-dirs :foreground nord9)
   (nord-mk-face 'helm-ff-dotted-director :foreground nord9 :underline t)
   (nord-mk-face 'helm-ff-dotted-symlink-director :foreground nord7 :
                 :weight 'bold)
   (nord-mk-face 'helm-ff-executable :foreground nord8)
   (nord-mk-face 'helm-ff-file :foreground nord4)
   (nord-mk-face 'helm-ff-invalid-symlink :foreground nord11 :weight 'bold)
   (nord-mk-face 'helm-ff-prefix :foreground nord0 :background nord9)
   (nord-mk-face 'helm-ff-symlink :foreground nord7)
   (nord-mk-face 'helm-grep-cmd-line :foreground nord4 :background nord0)
   (nord-mk-face 'helm-grep-file :foreground nord8)
   (nord-mk-face 'helm-grep-finish :foreground nord5)
   (nord-mk-face 'helm-grep-lineno :foreground nord4)
   (nord-mk-face 'helm-grep-match :inherit 'isearch)
   (nord-mk-face 'helm-grep-running :foreground nord8)
   (nord-mk-face 'helm-header :foreground nord9 :background nord2)
   (nord-mk-face 'helm-header-line-left-margin :foreground nord9
                 :background nord2)
   (nord-mk-face 'helm-history-deleted :foreground nord11)
   (nord-mk-face 'helm-history-remote :foreground nord4)
   (nord-mk-face 'helm-lisp-completion-info :foreground nord4 :weight 'bold)
   (nord-mk-face 'helm-lisp-show-completion :inherit 'isearch)
   (nord-mk-face 'helm-locate-finish :foreground nord14)
   (nord-mk-face 'helm-match :foreground nord8)
   (nord-mk-face 'helm-match-item :inherit 'isearch)
   (nord-mk-face 'helm-moccur-buffer :foreground nord8)
   (nord-mk-face 'helm-resume-need-update :foreground nord0
                 :background nord13)
   (nord-mk-face 'helm-selection :inherit 'highlight)
   (nord-mk-face 'helm-selection-line :background nord2)
   (nord-mk-face 'helm-source-header :height 1.44 :foreground nord8 :
                 :background nord2)
   (nord-mk-face 'helm-swoop-line-number-face :foreground nord4
                 :background nord0)
   (nord-mk-face 'helm-swoop-target-word-face :foreground nord0 :
                 :background nord7)
   (nord-mk-face 'helm-swoop-target-line-face :background nord13 :
                 :foreground nord3)
   (nord-mk-face 'helm-swoop-target-line-block-face :background nord13 :
                 :foreground nord3)
   (nord-mk-face 'helm-separator :background nord2)
   (nord-mk-face 'helm-visible-mark :background nord2)

   ;; > Magit
   (nord-mk-face 'magit-branch :foreground nord7 :weight 'bold)
   (nord-mk-face 'magit-diff-context-highlight :background nord2)
   (nord-mk-face 'magit-diff-file-header :foreground nord8 :box
                 `(:color ,(nord-get-color nord8)))
   (nord-mk-face 'magit-diffstat-added :foreground nord14)
   (nord-mk-face 'magit-diffstat-removed :foreground nord11)
   (nord-mk-face 'magit-hash :foreground nord8)
   (nord-mk-face 'magit-hunk-heading :foreground nord9)
   (nord-mk-face 'magit-hunk-heading-highlight :foreground nord9
                 :background nord2)
   (nord-mk-face 'magit-item-highlight :foreground nord8 :background nord2)
   (nord-mk-face 'magit-log-author :foreground nord7)
   (nord-mk-face 'magit-process-ng :foreground nord13 :weight 'bold)
   (nord-mk-face 'magit-process-ok :foreground nord14 :weight 'bold)
   (nord-mk-face 'magit-section-heading :foreground nord7 :weight 'bold)
   (nord-mk-face 'magit-section-highlight :background nord2)

   ;; > MU4E
   (nord-mk-face 'mu4e-header-marks-face :foreground nord9)
   (nord-mk-face 'mu4e-title-face :foreground nord8)
   (nord-mk-face 'mu4e-header-key-face :foreground nord8)
   (nord-mk-face 'mu4e-highlight-face :highlight)
   (nord-mk-face 'mu4e-flagged-face :foreground nord13)
   (nord-mk-face 'mu4e-unread-face :foreground nord13 :weight 'bold)
   (nord-mk-face 'mu4e-link-face :underline t)

   ;; > Powerline
   (nord-mk-face 'powerline-active1 :foreground nord4 :background nord1)
   (nord-mk-face 'powerline-active2 :foreground nord4 :background nord3)
   (nord-mk-face 'powerline-inactive1 :background nord2)
   (nord-mk-face 'powerline-inactive2 :background nord2)

   ;; > Powerline Evil
   (nord-mk-face 'powerline-evil-base-face :foreground nord4)
   (nord-mk-face 'powerline-evil-normal-face :background nord8)
   (nord-mk-face 'powerline-evil-insert-face :foreground nord0 :
                 :background nord4)
   (nord-mk-face 'powerline-evil-visual-face :foreground nord0 :
                 :background nord7)
   (nord-mk-face 'powerline-evil-replace-face :foreground nord0
                 :background nord9)

   ;; > NeoTree
   (nord-mk-face 'neo-banner-face :foreground nord10)
   (nord-mk-face 'neo-dir-link-face :foreground nord9)
   (nord-mk-face 'neo-expand-btn-face :foreground nord6 :bold t)
   (nord-mk-face 'neo-file-link-face :foreground nord4)
   (nord-mk-face 'neo-root-dir-face :foreground nord7 :weight 'bold)
   (nord-mk-face 'neo-vc-added-face :foreground nord14)
   (nord-mk-face 'neo-vc-conflict-face :foreground nord11)
   (nord-mk-face 'neo-vc-default-face :foreground nord4)
   (nord-mk-face 'neo-vc-edited-face :foreground nord13)
   (nord-mk-face 'neo-vc-ignored-face :foreground nord3)
   (nord-mk-face 'neo-vc-missing-face :foreground nord12)
   (nord-mk-face 'neo-vc-needs-merge-face :background nord12 :
                 :foreground nord4)
   (nord-mk-face 'neo-vc-needs-update-face :background nord10
                 :foreground nord4)
   (nord-mk-face 'neo-vc-removed-face :foreground nord11 :strike-through nil)
   (nord-mk-face 'neo-vc-up-to-date-face :foreground nord4)
   (nord-mk-face 'neo-vc-user-face :foreground nord4)

   ;; > Cider
   (nord-mk-face 'cider-result-overlay-face:background "unspecified")

   ;; > Org
   (nord-mk-face 'org-level-1 :foreground nord7 :weight 'extra-bold)
   (nord-mk-face 'org-level-2 :foreground nord8 :weight 'bold)
   (nord-mk-face 'org-level-3 :foreground nord9 :weight 'semi-bold)
   (nord-mk-face 'org-level-4 :foreground nord10 :weight 'normal)
   (nord-mk-face 'org-level-5 :inherit 'org-level-4)
   (nord-mk-face 'org-level-6 :inherit 'org-level-4)
   (nord-mk-face 'org-level-7 :inherit 'org-level-4)
   (nord-mk-face 'org-level-8 :inherit 'org-level-4)
   (nord-mk-face 'org-agenda-structure :foreground nord9)
   (nord-mk-face 'org-agenda-date :foreground nord8 :underline "unspecified")
   (nord-mk-face 'org-agenda-done :foreground nord14)
   (nord-mk-face 'org-agenda-dimmed-todo-face :background nord13)
   (nord-mk-face 'org-block :foreground nord4)
   (nord-mk-face 'org-block-background :background nord0)
   (nord-mk-face 'org-block-begin-line :foreground nord7)
   (nord-mk-face 'org-block-end-line :foreground nord7)
   (nord-mk-face 'org-checkbox :foreground nord9)
   (nord-mk-face 'org-checkbox-statistics-done :foreground nord14)
   (nord-mk-face 'org-checkbox-statistics-todo :foreground nord13)
   (nord-mk-face 'org-code :foreground nord7)
   (nord-mk-face 'org-column :background nord2)
   (nord-mk-face 'org-column-title :inherit 'org-column :weight 'bold
                 :underline t)
   (nord-mk-face 'org-date :foreground nord8)
   (nord-mk-face 'org-document-info :foreground nord4)
   (nord-mk-face 'org-document-info-keyword :foreground nord3 :weight 'bold)
   (nord-mk-face 'org-document-title :foreground nord8 :weight 'bold)
   (nord-mk-face 'org-done :foreground nord14 :weight 'bold)
   (nord-mk-face 'org-ellipsis :foreground nord3)
   (nord-mk-face 'org-footnote :foreground nord8)
   (nord-mk-face 'org-formula :foreground nord9)
   (nord-mk-face 'org-hide :foreground nord0 :background nord0)
   (nord-mk-face 'org-link :underline t)
   (nord-mk-face 'org-scheduled :foreground nord14)
   (nord-mk-face 'org-scheduled-previously :foreground nord13)
   (nord-mk-face 'org-scheduled-today :foreground nord8)
   (nord-mk-face 'org-special-keyword :foreground nord9)
   (nord-mk-face 'org-table :foreground nord9)
   (nord-mk-face 'org-todo :foreground nord13 :weight 'bold)
   (nord-mk-face 'org-upcoming-deadline :foreground nord12)
   (nord-mk-face 'org-warning :foreground nord13 :weight 'bold)
   (nord-mk-face 'font-latex-bold-face :inherit 'bold)
   (nord-mk-face 'font-latex-italic-face :slant 'italic)
   (nord-mk-face 'font-latex-string-face :foreground nord14)
   (nord-mk-face 'font-latex-match-reference-keywords :foreground nord9)
   (nord-mk-face 'font-latex-match-variable-keywords :foreground nord4)
   (nord-mk-face 'ido-only-match :foreground nord8)
   (nord-mk-face 'org-sexp-date :foreground nord7)
   (nord-mk-face 'ido-first-match :foreground nord8 :weight 'bold)
   (nord-mk-face 'ido-subdir :foreground nord9)
   (nord-mk-face 'org-quote :inherit 'org-block :slant 'italic)
   (nord-mk-face 'org-verse :inherit 'org-block :slant 'italic)
   (nord-mk-face 'org-agenda-date-weekend :foreground nord9)
   (nord-mk-face 'org-agenda-date-today :foreground nord8 :weight 'bold)
   (nord-mk-face 'org-agenda-done :foreground nord14)
   (nord-mk-face 'org-verbatim :foreground nord7)

   ;; > ivy-mode
   (nord-mk-face 'ivy-current-match :inherit 'region)
   (nord-mk-face 'ivy-minibuffer-match-face-1 :inherit 'default)
   (nord-mk-face 'ivy-minibuffer-match-face-2 :background nord7
                 :foreground nord0)
   (nord-mk-face 'ivy-minibuffer-match-face-3 :background nord8
                 :foreground nord0)
   (nord-mk-face 'ivy-minibuffer-match-face-4 :background nord9
                 :foreground nord0)
   (nord-mk-face 'ivy-remote :foreground nord14)
   (nord-mk-face 'ivy-posframe :background nord1)
   (nord-mk-face 'ivy-posframe-border :background nord1)
   (nord-mk-face 'ivy-remote :foreground nord14)

   ;; > perspective
   (nord-mk-face 'persp-selected-face :foreground nord8 :weight 'bold)))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nord)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; nord-theme.el ends here
