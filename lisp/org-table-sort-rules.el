;; NOTE: Column names don't work if they have regexp special characters

(defun org-table-sort-rules/collect-options (line)
  (string-match "#\\+SORT:[[:space:]]*\\(.*\\)" line)
  (let ((full-option-string (match-string 1 line))
        (search-start-idx 0)
        (sort-options '()))
    (while (string-match "\"\\([^\"]*\\)\":\\([antfANTF]\\)" full-option-string search-start-idx)
      (setq sort-options
            (cl-acons (match-string 1 full-option-string)
                      (string-to-char (match-string 2 full-option-string))
                      sort-options))
      (setq search-start-idx (match-end 0)))
    (reverse sort-options)))

(defun org-table-sort-rules/apply-sort-option (option-cons)
  "Call me with point at beginning of table."
  (search-forward-regexp (format "|[[:space:]]*%s[[:space:]]*|" (regexp-quote (car option-cons))))
  (backward-word)
  (next-line 2)
  (org-table-sort-lines nil (cdr option-cons)))

(defun org-table-sort-rules/apply ()
  (interactive)
  (let ((sort-options '())
        (column-names '())
        (original-point (point)))
    (goto-char (org-table-begin))
    ;; Collect options
    (save-excursion
      (while (and (equal 0 (forward-line -1))
                  (looking-at "#\\+"))
        (if (looking-at "#\\+SORT: ")
            (setf sort-options (org-table-sort-rules/collect-options (thing-at-point 'line))))))
    ;; Create column-names
    (let* ((column-name-line (thing-at-point 'line))
           (pre-column-names (mapcar 's-trim (split-string column-name-line "|"))))
      (setq column-names (remove "" pre-column-names)))
    ;; Check that all option columns are in column-list
    (dolist (option-cons sort-options)
      (if (not (member (car option-cons) column-names))
          (error "Option column '%s' not found in table" (car option-cons))))
    ;; Now apply the options in order
    (dolist (option-cons sort-options)
      (let ((start-point (point)))
        (org-table-sort-rules/apply-sort-option option-cons)
        (goto-char start-point)))
    (goto-char original-point)
    (if (not (null sort-options))
        (message "Applied sort rule")
      (message "No sort rule found"))))

(defun org-table-sort-rules/apply-all ()
  "Apply all the table sort rules in a buffer. Useful for hooks."
  (interactive)
  (let ((start-point (point)))
    (goto-char (point-min))
    (while (search-forward-regexp "^#\\+SORT:" nil t)
      (while (and (equal 0 (forward-line 1))
                  (looking-at "#\\+")))
      (org-table-sort-rules/apply))
    (goto-char start-point))
  (message "Applied all sort rules"))

(provide 'org-table-sort-rules)
