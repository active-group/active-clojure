(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, convert words after that
index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                     (downcase (match-string 0 s))) 
                             t nil s)))
    (downcase s)))


(defun field->field-tuple (field name)
  (list field
        " "
        (concat name "-" field)
        "\n   "))

(defun create-field-tuples (fields name)
  (let* ((field-tuples (mapcar (lambda (field)
                                 (field->field-tuple field name))
                               fields))
         (last-tuple (butlast (car (last field-tuples))))
         (field-tuples2 (append (butlast field-tuples)
                                (list last-tuple))))
    (apply #'concat (append '("[")
                            (apply #'append field-tuples2)
                            '("]")))))

(defun create-constructor (name)
  (concat "make-" name))

(defun create-predicate (name)
  (concat name "?"))

(defun insert-define-record-type ()
  (interactive)
  (let* ((type-name (read-string "type-name: "))
         (name (un-camelcase-string type-name))
         (fields (seq-filter (lambda (elt) (not (string= "" elt))) (split-string (read-string "fields: ") " ")))
         (field-tuples (create-field-tuples fields name)))
    (insert (concat "(define-record-type " type-name
                  "\n  "
                  (create-constructor name)
                  "\n  "
                  (create-predicate name)
                  "\n  "
                  field-tuples ")"))))
