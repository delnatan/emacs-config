;; DE_fun01.el --- Custom-defined functions for various text-related tasks -*- lexical-binding: t -*-

(defun de/wrap-region (n start end)
  "Wrap text in current region.
(wrap-region N START END)

This function is /interactive/.The argument N is supplied by prefix (C-u). START
and END is taken from a regions' mark and point, respectively.This function
replaces currently selected region

Typically used to tidy long DNA/protein sequence.  Pass a prefix before running
command. For example:

C-u 50 M-x wrap-region

"
  ;; get prefix and region into arguments
  (interactive "p\nr")
  (if (use-region-p)
      ;; if using region
      (save-excursion
	(let* ((regionp (clean-string (buffer-substring start end)))
	       (result
		(string-join (split-string-every regionp n) "\n")))
	  ;; `let` body
	  (kill-region start end) ;; delete current region
	  (insert result) ;; insert split text
	  )
	;; otherwise
	(message "no region was selected"))
    )
  )

(defun de/wrap-region-keep-punctuations (n start end)
  "Wrap text in current region.
(wrap-region N START END)

This function is /interactive/.The argument N is supplied by prefix (C-u). START
and END is taken from a regions' mark and point, respectively.This function
replaces currently selected region

Typically used to tidy long DNA/protein sequence.  Pass a prefix before running
command. For example:

C-u 50 M-x wrap-region

"
  ;; get prefix and region into arguments
  (interactive "p\nr")
  (if (use-region-p)
      ;; if using region
      (save-excursion
	(let* ((regionp (clean-string-keep-punctuations (buffer-substring start end)))
	       (result
		(string-join (split-string-every regionp n) "\n")))
	  ;; `let` body
	  (kill-region start end) ;; delete current region
	  (insert result) ;; insert split text
	  )
	;; otherwise
	(message "no region was selected"))
    )
  )



(defun de/copy-cleaned-region (start end)
  "run clean-string() on current region and put into kill-ring"
  (interactive "r")
  (if (use-region-p)
      (kill-new (clean-string (buffer-substring start end)))
    (message "no region was selected")
    ))

(defun de/subset-cleaned-region (start end)
  "run clean-string() on current region and take subset from
string's start (sstart) and end (send)"
  (interactive "r")
  (if (use-region-p)
      (let* ((regionp (clean-string (buffer-substring start end)))
	     (sstart (string-to-number (read-from-minibuffer "String start: ")))
	     (send (string-to-number (read-from-minibuffer (format "String end (%d): " (length regionp))))))
	(message "Put %d to %d (%d total chars) to kill-ring" sstart send (length regionp))
	(kill-new (substring regionp sstart send))
	)
    (message "no region was selected")))


(defun cleaned-region-length (start end)
  "run clean-string() and count the number of characters.
Useful for counting the number of characters in a region with linebreaks.
Meant to be used with biosequences."
  (interactive "r")
  (if (use-region-p)
      (message "Region has %d characters" (length (clean-string (buffer-substring start end))))
    (message "no region was selected")))

(defun split-string-every (string chars)
  "split STRING into substrings of length CHARS character

This returns a list of strings."
  (cond ((string-empty-p string) nil)
	((< (length string) chars)
	 (list string))
	(t (cons (substring string 0 chars)
		 (split-string-every (substring string chars)
				     chars)))))

(defun clean-string (string)
  "cleans input STRING

These characters are removed:
numbers, punctuation marks (non-word), whitespace, newline"
  (replace-regexp-in-string "[[:digit:][:punct:]\s\n]" "" string))

(defun clean-string-keep-punctuations (string)
  "cleans input STRING

These characters are removed:
numbers, punctuation marks (non-word), whitespace, newline"
  (replace-regexp-in-string "[[:digit:]\s\n]" "" string))


(defun de/sum-list (lst)
  "adds up every element of a list using recursion

Usage: (sum-list \\='(1 2 3 4 5)) returns 15
"
  (if (null lst)
      0
    (+ (car lst) (sum-list (cdr lst)))))


(defun de/print-exec-path ()
  "Print the contents of exec-path into current buffer"
  (interactive)
  (dolist (path exec-path)
    (insert (concat path "\n"))))


(defun de/comment-fence ()
  "Inserts a 'fenced' comment from the current point to the fill-column."
  (interactive)
  (let* ((current-column (current-column))
         (fill-column (or fill-column 80))
         (n (- fill-column current-column)))
    (insert (make-string n ?#))))


(defun de/comment-at-end ()
  "Places a '#' at fill-column minus one for the current line or each line in the selected region."
  (interactive)
  (if (use-region-p)
      ;; If a region is selected, apply to each line in the region
      (let ((start (region-beginning))
            (end (region-end))
            (fill-column (or fill-column 80))) ; Use fill-column or default to 80
        (save-excursion
          (goto-char start)
          (let ((end-marker (copy-marker end))) ; Use a marker for end to adjust position automatically
            (while (< (point) end-marker)
              (let ((line-end (save-excursion (end-of-line) (point))))
                (if (< (current-column) (- fill-column 1))
                    (progn
                      (end-of-line)
                      (if (> (- fill-column 1) (current-column))
                          (insert (make-string (- (- fill-column 1) (current-column)) ? ))))
                  (delete-region (+ (line-beginning-position) fill-column -1) line-end))
                (end-of-line)
                (unless (or (= (char-before) ?#) (> (current-column) fill-column))
                  (insert "#")))
              (forward-line 1)))))
    ;; If no region is selected, apply to the current line
    (save-excursion
      (let* ((target-column (- fill-column 1))
             (current-length (progn (end-of-line) (current-column))))
        (if (< current-length target-column)
            (insert (make-string (- target-column current-length) ? ) "#")
          (goto-char (+ (line-beginning-position) target-column))
          (unless (= (char-before) ?#)
            (delete-region (point) (progn (end-of-line) (point)))
            (insert "#")))))))
