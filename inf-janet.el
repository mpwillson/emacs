;;; inf-janet.el --- an inferior-janet mode -*- lexical-binding: t; -*-

;; This mode is copied from inf-clojure.el and lightly modified for Janet.
;;
;; Janet modifications Copyright (C) Mark Willson

;; Copyright (C) Bozhidar Batsov

;; Authors: Bozhidar Batsov <bozhidar@batsov.com>
;;       Olin Shivers <shivers@cs.cmu.edu>
;; URL: http://github.com/janet-emacs/inf-janet
;; Keywords: processes, janet
;; Version: 20141204.820
;; X-Original-Version: 1.0.0-cvs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; inf-lisp adapted for Janet.
;;
;; If you're installing manually, you'll need to:
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;
;;    (autoload 'inf-janet "inf-janet" "Run an inferior Janet process" t)
;;    (add-hook 'janet-mode-hook 'inf-janet-minor-mode)

;; Changes to support Janet:
;;
;;   1. clojure => janet
;;   2. Removed unsupported features (e.g. var doc, args doc)
;;   3. Filter comint output to remove Janet subprompts and prompts
;;      caused by comments or blank lines.
;;   4. Replace load-file by import*

;;; Code:

(require 'comint)
(require 'janet-mode)


(defgroup inf-janet nil
  "Run an outside Janet in an Emacs buffer."
  :group 'janet)

(defcustom inf-janet-prompt-read-only nil
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'inf-janet)

(defcustom inf-janet-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior Janet's input history.
Input matching this regexp is not saved on the input history in Inferior Janet
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inf-janet)

(defvar inf-janet-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-x\C-e" 'janet-eval-last-sexp)
    (define-key map "\C-c\C-l" 'janet-load-file)
    (define-key map "\C-c\C-v" 'janet-show-documentation)
    map))

(easy-menu-define
  inf-janet-menu
  inf-janet-mode-map
  "Inferior Janet Menu"
  '("Inf-Janet"
    ["Eval Last Sexp" janet-eval-last-sexp t]
    "--"
    ["Load File..." janet-load-file t]
    "--"
    ["Show Documentation for Var..." janet-show-documentation t]))

(defvar inf-janet-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x"  'janet-eval-defun)     ; Gnu convention
    (define-key map "\C-x\C-e" 'janet-eval-last-sexp) ; Gnu convention
    (define-key map "\C-c\C-e" 'janet-eval-defun)
    (define-key map "\C-c\C-r" 'janet-eval-region)
    (define-key map "\C-c\C-n" 'janet-eval-form-and-next)
    (define-key map "\C-c\C-p" 'janet-eval-paragraph)
    (define-key map "\C-c\C-z" 'switch-to-janet)
    (define-key map "\C-c\C-l" 'janet-load-file)
    (define-key map "\C-c\C-v" 'janet-show-documentation)
    map))

;;;###autoload
(define-minor-mode inf-janet-minor-mode
  "Minor mode for interacting with the inferior Janet process buffer.

The following commands are available:

\\{inf-janet-minor-mode-map}"
  :lighter "" :keymap inf-janet-minor-mode-map)

(defcustom inf-janet-program "janet -s"
  "Program name for invoking an inferior Janet in Inferior Janet mode."
  :type 'string
  :group 'inf-janet)

(defcustom inf-janet-load-command "(import* \"/%s\")\n"
  "Format-string for building a Janet expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Janet expression that will command the inferior Janet
to load that file.~"
  :type 'string
  :group 'inf-janet)

(defcustom inf-janet-prompt "repl:[:digit:]+:> "
  "Regexp to recognize prompts in the Inferior Janet mode."
  :type 'regexp
  :group 'inf-janet)

(defvar inf-janet-buffer nil "The current inf-janet process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Janet processes, you start the first up
with \\[inf-janet].  It will be in a buffer named `*inf-janet*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inf-janet].  It will be in a new buffer,
named `*inf-janet*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Janet processes --
like `janet-eval-defun' or `janet-show-arglist' -- have to choose a process
to send to, when you have more than one Janet process around.  This
is determined by the global variable `inf-janet-buffer'.  Suppose you
have three inferior Janets running:
    Buffer              Process
    foo                 inf-janet
    bar                 inf-janet<2>
    *inf-janet*     inf-janet<3>
If you do a \\[janet-eval-defun] command on some Janet source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inf-janet*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inf-janet-buffer'.
This process selection is performed by function `inf-janet-proc'.

Whenever \\[inf-janet] fires up a new process, it resets
`inf-janet-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you might need to change `inf-janet-buffer' to
whichever process buffer you want to use.")

(defvar inf-janet-mode-hook '()
  "Hook for customizing Inferior Janet mode.")

(put 'inf-janet-mode 'mode-class 'special)

(define-derived-mode inf-janet-mode comint-mode "Inferior Janet"
  "Major mode for interacting with an inferior Janet process.
Runs a Janet interpreter as a subprocess of Emacs, with Janet I/O through an
Emacs buffer.  Variable `inf-janet-program' controls which Janet interpreter
is run.  Variables `inf-janet-prompt', `inf-janet-filter-regexp' and
`inf-janet-load-command' can customize this mode for different Janet
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable `inf-janet-buffer'.

\\{inf-janet-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inf-janet-mode-hook' (in that order).

You can send text to the inferior Janet process from other buffers containing
Janet source.
    `switch-to-janet' switches the current buffer to the Janet process buffer.
    `janet-eval-defun' sends the current defun to the Janet process.
    `janet-eval-region' sends the current region to the Janet process.

    Prefixing the janet-eval/defun/region commands with
    a \\[universal-argument] causes a switch to the Janet process buffer after sending
    the text.

Commands:\\<inf-janet-mode-map>
\\[comint-send-input] after the end of the process' output sends the text from the
    end of process to point.
\\[comint-send-input] before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
\\[comint-copy-old-input] copies the sexp ending at point to the end of the process' output,
    allowing you to edit it before sending it.
If `comint-use-prompt-regexp' is nil (the default), \\[comint-insert-input] on old input
   copies the entire old input to the end of the process' output, allowing
   you to edit it before sending it.  When not used on old input, or if
   `comint-use-prompt-regexp' is non-nil, \\[comint-insert-input] behaves according to
   its global binding.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
\\[janet-indent-line] indents for Janet; with argument, shifts rest
    of expression rigidly with the current line.
\\[indent-sexp] does \\[janet-indent-line] on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (setq comint-prompt-regexp inf-janet-prompt)
  (setq mode-line-process '(":%s"))
  ;;(janet-mode-variables)
  (setq comint-get-old-input (function janet-get-old-input))
  (setq comint-input-filter (function janet-input-filter))
  (set (make-local-variable 'comint-prompt-read-only) inf-janet-prompt-read-only)
  (add-hook 'completion-at-point-functions 'inf-janet-completion-at-point nil t))

(defun janet-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun janet-input-filter (str)
  "t if STR does not match `inf-janet-filter-regexp'."
  (not (string-match inf-janet-filter-regexp str)))

(defun janet-re-seq (regexp string)
  "Rwturn list of all regexp matches in a string"
  (if (= (length string) 0)
      '()
    (save-match-data
      (let ((pos 0)
            matches)
        (while (string-match regexp string pos)
          (push (match-string 0 string) matches)
          (setq pos (match-end 0)))
        matches))))

(defun janet-strip-prompt (response)
  "Strip sub-prompts from reponse. Extract response payload and latest prompt
 to return as Janet output."
  (let ((str (replace-regexp-in-string "repl:[0-9]+:[[(`{\"]+> " "" response)))
    (let ((payload (replace-regexp-in-string "repl:[0-9]+:> " "" str))
           (prompts (janet-re-seq "\\(repl:[0-9]+:> \\)$" str)))
      (concat (replace-regexp-in-string "\n$" "" payload) "\n" (car prompts)))))

;;;###autoload
(defun inf-janet (cmd)
  "Run an inferior Janet process, input and output via buffer `*inf-janet*'.
If there is a process already running in `*inf-janet*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inf-janet-program').  Runs the hooks from
`inf-janet-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
                         (read-string "Run Janet: " inf-janet-program)
                       inf-janet-program)))
  (if (not (comint-check-proc "*inf-janet*"))
      (let ((cmdlist (split-string cmd)))
        (set-buffer (apply (function make-comint)
                           "inf-janet" (car cmdlist) nil (cdr cmdlist)))
        (inf-janet-mode)))
  (setq inf-janet-buffer "*inf-janet*")
  (add-hook 'comint-preoutput-filter-functions 'janet-strip-prompt)
  (pop-to-buffer inf-janet-buffer)
  (other-window -1))
;;  (pop-to-buffer-same-window "*inf-janet*"))

;;;###autoload
(defalias 'run-janet 'inf-janet)

(defun janet-eval-paragraph (&optional and-go)
  "Send the current paragraph to the inferior Janet process.
Prefix argument means switch to the Janet buffer afterwards."
  (interactive "P")
  (save-excursion
    (mark-paragraph)
    (janet-eval-region (point) (mark) and-go)))

(defun janet-eval-region (start end &optional and-go)
  "Send the current region to the inferior Janet process.
Prefix argument means switch to the Janet buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inf-janet-proc) start end)
  (comint-send-string (inf-janet-proc) "\n")
  (if and-go (switch-to-janet t)))

(defun janet-eval-string (string)
  "Send the string to the inferior Janet process to be executed."
  (comint-send-string (inf-janet-proc) (concat string "\n")))

(defun janet-do-defun (do-string do-region)
  "Send the current defun to the inferior Janet process.
The actually processing is done by `do-string' and `do-region'
 which determine whether the code is compiled before evaluation.
DEFVAR forms reset the variables to the init values."
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (funcall do-region (point) end))))

(defun janet-eval-defun (&optional and-go)
  "Send the current defun to the inferior Janet process.
DEFVAR forms reset the variables to the init values.
Prefix argument means switch to the Janet buffer afterwards."
  (interactive "P")
  (janet-do-defun 'janet-eval-string 'janet-eval-region)
  (if and-go (switch-to-janet t)))

(defun janet-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Janet process.
Prefix argument means switch to the Janet buffer afterwards."
  (interactive "P")
  (janet-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun janet-eval-form-and-next ()
  "Send the previous sexp to the inferior Janet process and move to the next one."
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (janet-eval-last-sexp)
  (forward-sexp))

(defun switch-to-janet (eob-p)
  "Switch to the inferior Janet process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-janet-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inf-janet-buffer t))))
        (pop-to-buffer inf-janet-buffer))
    (run-janet inf-janet-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))


;;; Now that janet-eval-/defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun janet-eval-region-and-go (start end)
  "Send the current region to the inferior Janet, and switch to its buffer."
  (interactive "r")
  (janet-eval-region start end t))

(defun janet-eval-defun-and-go ()
  "Send the current defun to the inferior Janet, and switch to its buffer."
  (interactive)
  (janet-eval-defun t))

(defvar janet-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `janet-load-file' command.")

(defcustom janet-source-modes '(janet-mode)
  "Used to determine if a buffer contains Janet source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Janet source file by `janet-load-file' and `janet-compile-file'.
Used by these commands to determine defaults."
  :type '(repeat symbol)
  :group 'inf-janet)

(defun janet-load-file (file-name)
  "Load a Janet file into the inferior Janet process."
  (interactive (comint-get-source "Load Janet file: " janet-prev-l/c-dir/file
                                  janet-source-modes nil)) ; nil because LOAD
                                        ; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq janet-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (inf-janet-proc)
                      (format inf-janet-load-command
                              (string-remove-suffix
                               ".janet" (file-name-nondirectory file-name))))
  (switch-to-janet t))


;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command strings
;;; ===============

(defvar janet-doc-command
  "(doc %s)\n"
  "Command to query inferior Janet for a var's documentation.")


;;; Ancillary functions
;;; ===================

;;; Reads a string from the user.
(defun janet-symprompt (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))


;;; Adapted from function-called-at-point in help.el.
(defun janet-fn-called-at-pt ()
  "Returns the name of the function called in the current call.
The value is nil if it can't find one."
  (condition-case nil
      (save-excursion
        (save-restriction
          (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
          (backward-up-list 1)
          (forward-char 1)
          (let ((obj (read (current-buffer))))
            (and (symbolp obj) obj))))
    (error nil)))


;;; Adapted from variable-at-point in help.el.
(defun janet-var-at-pt ()
  (condition-case ()
      (save-excursion
        (forward-sexp -1)
        (skip-chars-forward "'")
        (let ((obj (read (current-buffer))))
          (and (symbolp obj) obj)))
    (error nil)))


;;; Documentation functions: var doc and arglist.
;;; ======================================================================

(defun janet-show-documentation (var)
  "Send a command to the inferior Janet to give documentation for VAR.
See variable `janet-var-doc-command'."
  (interactive (janet-symprompt "fn doc" (janet-var-at-pt)))
  (comint-proc-query (inf-janet-proc) (format janet-doc-command var)))


;;  "Returns the current inferior Janet process.
;; See variable `inf-janet-buffer'."
(defun inf-janet-proc ()
  (let ((proc (get-buffer-process (if (derived-mode-p 'inf-janet-mode)
                                      (current-buffer)
                                    inf-janet-buffer))))
    (or proc
        (error "No Janet subprocess; see variable `inf-janet-buffer'"))))

(defun inf-janet-completions (expr)
  "Return a list of completions for the Janet expression starting with EXPR."
  (let* ((proc (inf-janet-proc))
         (line (buffer-substring (save-excursion (move-beginning-of-line 1)
                                                 (point))
                                 (point)))
         (comint-filt (process-filter proc))
         (kept "")
         completions)
    (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
    (unwind-protect
        (let ((completion-snippet
               (format
                janet-completion-command (substring-no-properties expr))))
          (process-send-string proc completion-snippet)
          (while (and (not (string-match inf-janet-prompt kept))
                      (accept-process-output proc 2)))
          (setq completions (read kept))
          ;; Subprocess echoes output on Windows and OS X.
          (when (and completions (string= (concat (car completions) "\n") completion-snippet))
            (setq completions (cdr completions))))
      (set-process-filter proc comint-filt))
    completions))

(defconst inf-janet-janet-expr-break-chars " \t\n\"\'`><,;|&{(")

(defun inf-janet-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward (concat "^" inf-janet-janet-expr-break-chars))
        (cons (point) end)))))

(defun inf-janet-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (inf-janet-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring (car bounds) (cdr bounds)))))

(defun inf-janet-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (let ((bounds (inf-janet-completion-bounds-of-expr-at-point)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (if (fboundp 'completion-table-with-cache)
                (completion-table-with-cache #'inf-janet-completions)
              (completion-table-dynamic #'inf-janet-completions))))))

;;;###autoload
(dolist (mode janet-source-modes)
  (add-hook (intern (format "%s-hook" mode)) 'inf-janet-minor-mode))

(provide 'inf-janet)

;;; inf-janet.el ends here
