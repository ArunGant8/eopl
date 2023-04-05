(define record-proc-names
  (lambda (name fields)
    (let ((name-str (symbol->string name)))
      (cons (string->symbol (string-append (symbol->string
                                             'make-) name-str))
            (cons (string->symbol (string-append name-str "?"))
                  (map (lambda (field)
                         (string->symbol (string-append name-str
                                                        "-"
                                                        (symbol->string
                                                          field))))
                       fields))))))

;;; NOTE: extend-syntax, though supported in my version of
;;; Chez-scheme (9.5.8), is deprecated in favour of
;;; define-syntax and syntax-rules. Learn those when time
;;; permits.
 
(extend-syntax (variant-case else)
               ((variant-case var) (error 'variant-case
                                          "variant-case: no clause matches"
                                          var))
               ((variant-case var (else exp1 exp2 ...))
                (begin exp1 exp2 ...))
               ((variant-case exp clause ...)
                (not (symbol? 'exp))
                (with ((var (gensym)))
                      (let ((var exp)) (variant-case var clause ...))))
               ((variant-case var (name (field ...) exp1 exp2 ...)
                              clause ...)
                (with (((make-name name? name-field ...)
                        (record-proc-names 'name '(field ...))))
                      (if (name? var)
                        (let ((field (name-field var)) ...)
                          exp1 exp2 ...)
                        (variant-case var clause ...)))))
