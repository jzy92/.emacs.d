(defun org-table-sort-rules/collect-options (sort-options line)
  (string-match "#\\+SORT:[[:space:]]*\\(.*\\)" line)
  (let ((new-option-strings (split-string (match-string 1 line) "[[:space:]]+")))
    (setq sort-options
          (mapcar (lambda (option-string)
                    (if (string-match "\"\\(.*\\)\":\\([antfANTF]\\)" option-string)
                        (cons (match-string 1 option-string) (string-to-char (match-string 2 option-string)))
                      (error "Couldn't extract valid sort option from string %s" option-string)))
          new-option-strings))
    sort-options))

(defun org-table-sort-rules/apply-sort-option (option-cons)
  "Call me with point at beginning of table."
  (search-forward-regexp (format "|[[:space:]]*%s[[:space:]]*|" (car option-cons)))
  (backward-word)
  (next-line 2)
  (org-table-sort-lines nil (cdr option-cons)))

(defun org-table-sort-rules/apply ()
  (interactive)
  (goto-char (org-table-begin))
  (let ((sort-options '())
        (column-names '()))
    ;; Collect options
    (save-excursion
      (while (and (equal 0 (forward-line -1))
                  (looking-at "#\\+"))
        (if (looking-at "#\\+SORT: ")
            (setf sort-options (org-table-sort-rules/collect-options sort-options (thing-at-point 'line))))))
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
        (goto-char start-point)))))

(provide 'org-table-sort-rules)
