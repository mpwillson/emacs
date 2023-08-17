;;;;
;; global-replace is an attempt to simulate the function of the vi "g"
;; command.  Considers the whole buffer and replaces all occurances of
;; replace-re by replacement-str, in lines matching filter-re.
;;
;; mpw 2004/01/13

(defun global-replace-get-eol ()
  "Return position of end of current line."
  (save-excursion
    (end-of-line)
    (point)))

(defun global-replace(filter-re replace-re replacement-str)
  "Emulate vi g command"
  (interactive "MFilter regexp: \nMReplace regexp: \nMReplace with: ")
  (if (and (string= filter-re "") (string= replace-re ""))
      (error "Filter and replace regexps cannot both be empty")
    (if (string= filter-re "")
        (setq filter-re replace-re))
    (if (string= replace-re "")
        (setq replace-re filter-re))
    (save-excursion
      (goto-char (point-min))
      (let ((cnt 0) (save-case case-fold-search) end)
        (setq case-fold-search '())       ; case sensitive
        (while (and (not (eobp)) (re-search-forward filter-re nil t))
          (setq end (global-replace-get-eol))
          (beginning-of-line)
          (catch 'at-eol
            (while (re-search-forward replace-re end t)
              (replace-match replacement-str)
              (setq cnt (1+ cnt))
              (setq end (global-replace-get-eol))
              (when (= end (point))
                (throw 'at-eol nil))))
          (forward-line))
        (setq case-fold-search save-case)
        (message "Replaced %d strings" cnt)))))
