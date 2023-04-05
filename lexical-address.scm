;;; 3.3.2>
;;; Extend the language of 2.3.10 to include cond expressions. Adapt
;;; lexical-address so that it recognizes cond expressions.
;;; The subset of scheme we consider is given as follows:
;;; <exp> := <varref>
;;;       | (if <exp> <exp> <exp>)
;;;       | (lambda ({<var>}*) <exp>)
;;;       | ({<exp>}+)
;;;       | (let ({(<var> <exp>)}+) <exp>)
;;;       | (letrec ({(<var> <exp>)}+) <exp>)
;;;       | (cond {(<exp> <exp>)}+ {(else <exp>)}?)

(define lexical-address
  (lambda (le)
    (if (pair? (free-vars le))
      (caddr (lexical-address-subst
              (list 'lambda (free-vars le) le)
              (list (free-vars le))))
      (lexical-address-subst le '()))))

(define lexical-address-subst
  (lambda (le larglst)
    (cond 
      ((null? le) '())
      ((symbol? le) (find-lexical-address le larglst 0))
      ((eq? (car le) 'lambda) 
       (list 'lambda (cadr le)
             (lexical-address-subst (caddr le)
                                    (append (list (cadr le)) larglst))))
      ((eq? (car le) 'if)
       (list 'if
             (lexical-address-subst (cadr le) larglst)
             (lexical-address-subst (caddr le) larglst)
             (lexical-address-subst (cadddr le) larglst)))
      ((eq? (car le) 'let)
       (let ((asslst (cadr le))
             (body (caddr le)))
         (let ((varexp (split asslst '() '())))
           (let ((varlist (car varexp))
                 (explist (cadr varexp)))
             (list 'let (map
                          (lambda (var-exp)
                            (list (car var-exp)
                                  (lexical-address-subst
                                    (cadr var-exp) larglst)))
                          asslst)
                   (lexical-address-subst body
                                          (append (list varlist)
                                                  larglst)))))))
      ((eq? (car le) 'letrec)
       (let ((asslst (cadr le))
             (body (caddr le)))
         (let ((varexp (split asslst '() '())))
           (let ((varlist (car varexp))
                 (explist (cadr varexp)))
             (let ((newlarglst (append (list varlist) larglst)))
               (list 'letrec (map
                               (lambda (var-exp)
                                 (list (car var-exp)
                                       (lexical-address-subst
                                         (cadr var-exp) newlarglst)))
                               asslst)
                     (lexical-address-subst body newlarglst)))))))
      ((eq? (car le) 'cond)
       (cons 'cond (map (lambda (pair)
                          (if (eq? (car pair) 'else)
                            (list 'else (lexical-address-subst (cadr pair) larglst))
                            (list (lexical-address-subst (car pair) larglst)
                                  (lexical-address-subst (cadr pair) larglst))))
                        (cdr le))))
      ((pair? le)
       (map (lambda (lexp)
              (lexical-address-subst lexp larglst))
            le)))))

(define find-lexical-address
  (lambda (sym larglst d)
    (if (eq? -1 (list-index sym (car larglst)))
      (find-lexical-address sym (cdr larglst) (+ d 1))
      (list sym ': d (list-index sym (car larglst))))))

;;; Helper functions

(define list-index
  (lambda (s los)
    (if (null? los)
      -1
      (normalize (get-index s los) (length los)))))

(define get-index
  (lambda (s los)
    (if (null? los)
      0
      (if (eq? s (car los))
        0
        (+ 1 (get-index s (cdr los)))))))

(define normalize
  (lambda (index len)
    (if  (< index len)
      index
      -1)))

(define free-vars
  (lambda (le)
    (remove-dups (list-free-vars le '()))))

(define list-free-vars
  (lambda (le los)
    (cond
      ((null? le)
       '())
      ((symbol? le)
       (if (belongs le los)
         '()
         (list le)))
      ((eq? (car le) 'lambda)
       (list-free-vars (caddr le) (append (cadr le) los)))
      ((eq? (car le) 'if)
       (append (list-free-vars (cadr le) los)
               (list-free-vars (caddr le) los)
               (list-free-vars (cadddr le) los)))
      ((eq? (car le) 'let)
       (let ((asslst (cadr le))
             (body (caddr le)))
         (let ((varexp (split asslst '() '())))
           (let ((varlist (car varexp))
                 (explist (cadr varexp)))
             (append (list-free-vars body (append varlist los))
                     (apply append (map
                                     (lambda (lexp)
                                       (list-free-vars lexp los))
                                     explist)))))))
      ((eq? (car le) 'letrec)
       (let ((asslst (cadr le))
             (body (caddr le)))
         (let ((varexp (split asslst '() '())))
           (let ((varlist (car varexp))
                 (explist (cadr varexp)))
             (apply append (map
                             (lambda (lexp)
                               (list-free-vars lexp
                                               (append varlist los)))
                             (cons body explist)))))))
      ((eq? (car le) 'cond)
       (apply append (map
                       (lambda (lexp)
                         (list-free-vars lexp los))
                       (remove 'else (apply append (cdr le))))))
      ((pair? le)
       (apply append (map 
                       (lambda (lexp) 
                         (list-free-vars lexp los))
                       le))))))

(define belongs
  (lambda (sym los)
    (if (null? los)
      #f
      (if (eq? (car los) sym)
        #t
        (belongs sym (cdr los))))))

(define remove-dups
  (lambda (los)
    (if (null? los)
      '()
      (cons (car los) (remove (car los) (remove-dups (cdr los)))))))

(define remove
  (lambda (s los)
    (if (null? los)
      '()
      (if (eq? s (car los))
        (remove s (cdr los))
        (cons (car los) (remove s (cdr los)))))))
