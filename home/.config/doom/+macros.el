;;; ~/.my/dir/file.el -*- lexical-binding: t; -*-

(defalias 'λ 'lambda)

;; From https://gist.github.com/cbowdon/012d623920bd28453bf8
(defmacro template (text)
  "Expand text like \"Hello <<name>>\" to (format \"Hello %s\" name)."
  (let ((pattern "<<\\(.*?\\)>>"))
    ;; The regexp matches anything between delimiters, non-greedily
    (with-temp-buffer
      (save-excursion (insert text))
      (let ((matches '()))
        (while (re-search-forward pattern nil t)
          (push (match-string 1) matches)
          (replace-match "%s" t t))
        `(format ,(buffer-string) ,@(reverse (mapcar 'read matches)))))))
