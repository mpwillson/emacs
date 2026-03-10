# Emacs utilities

## global-replace.el

global-replace is an attempt to simulate the function of the vi "g"
command.  Considers the whole buffer and replaces all occurances of
replace-re by replacement-str, in lines matching filter-re.

If you're installing manually, you'll need to drop the file somewhere
on your load path (perhaps ~/.emacs.d)

## inf-janet.el

Yet another inferior mode for Janet. Copied from inf-clojure.el (by
Bozhidar Batsov) and lightly editted to work with Janet.

Dependent on janet-mode: <https://github.com/ALSchwalm/janet-mode>

### Changes to support Janet:

1. clojure => janet
2. Removed unsupported features (e.g. var doc, args doc)
3. Filter comint output to remove Janet subprompts and prompts related
   to comments or blank lines.
4. Replace load-file by import*

### Install

If you're installing manually, you'll need to:

* drop the file somewhere on your load path (perhaps ~/.emacs.d)
* Add the following lines to your .emacs file:

        (autoload 'inf-janet "inf-janet" "Run an inferior Janet process" t)
        (add-hook 'janet-mode-hook 'inf-janet-minor-mode)

## nord-theme.el

This is a version of the nord custom theme for Emacs. It is based on
the offical version at <https://github.com/nordtheme/emacs>, which now
appears dormant. This version fixes a number of issues, discussed at
<https://github.com/nordtheme/emacs/issues/59>.

Place a copy of nord-theme.el in your custom-themes directory
(e.g. `~/.emacs.d/custom-themes`).
