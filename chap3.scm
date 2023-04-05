;;; 3.1.2>
;;; Write let->application, which takes a let expression, represented
;;; as a list, and returns the equivalent expression, also represented
;;; as a list: an application of a procedure created by a lambda
;;; expression. The solution should not change the body of the let
;;; expression.
(define let->application
  (lambda (let-exp)
    (let ((asslst (cadr let-exp))
          (body (caddr let-exp)))
      (let ((varexp (split asslst '() '())))
        (let ((varlist (car varexp))
              (explist (cadr varexp)))
          (append (list (list 'lambda varlist
                      body))
                explist))))))

(define split
  (lambda (assignment-lst varlist explist)
    (if (null? assignment-lst)
      (list varlist explist)
      (split (cdr assignment-lst)
             (append varlist (list (caar assignment-lst)))
             (append explist (list (cadar assignment-lst)))))))

;;; 3.1.3>
;;; Rewrite subst using letrec
(define subst
  (lambda (new old slst)
    (letrec ((subst-symbol-expression
               (lambda (se)
                 (if (symbol? se)
                   (if (eq? se old) new se)
                   (subst new old se)))))
      (if (null? slst)
        '()
        (cons (subst-symbol-expression (car slst))
              (subst new old (cdr slst)))))))

;;; 3.1.4>
;;; The special forms let and letrec are both binding forms. Extend the
;;; definition of 'occurs free' and 'occurs bound' to accomodate let and
;;; letrec expressions. Augment your programs 'free-vars' and 'bound-vars'
;;; to take your new rules into account.

;;; MODIFY THE FOLLOWING CODE:

(define free-vars
  (lambda (le)
    (remove-dups (list-free-vars le '()))))

(define list-free-vars
  (lambda (le los)
    (if (null? le)
      '()
      (if (symbol? le)
        (if (belongs le los)
          '()
          (list le))
        (if (eq? (car le) 'lambda)
          (list-free-vars (caddr le) (append (cadr le) los))
          (if (eq? (car le) 'if)
            (append (list-free-vars (cadr le) los)
                    (list-free-vars (caddr le) los)
                    (list-free-vars (cadddr le) los))
            (if (eq? (car le) 'let)
              (let ((asslst (cadr le))
                    (body (caddr le)))
                (let ((varexp (split asslst '() '())))
                  (let ((varlist (car varexp))
                        (explist (cadr varexp)))
                    (append (list-free-vars body (append varlist los))
                            (apply append (map
                                            (lambda (lexp)
                                              (list-free-vars lexp los))
                                            explist))))))
              (if (eq? (car le) 'letrec)
                (let ((asslst (cadr le))
                      (body (caddr le)))
                  (let ((varexp (split asslst '() '())))
                    (let ((varlist (car varexp))
                          (explist (cadr varexp)))
                      (apply append (map
                                      (lambda (lexp)
                                        (list-free-vars lexp
                                                        (append varlist los)))
                                      (cons body explist))))))
                (if (pair? le)
                  (apply append (map 
                                  (lambda (lexp) 
                                    (list-free-vars lexp los))
                                  le)))))))))))

(define bound-vars
  (lambda (le)
    (remove-dups (list-bound-vars le))))

(define list-bound-vars
  (lambda (le)
    (if (null? le)
      '()
      (if (symbol? le)
        '()
        (if (eq? (car le) 'lambda)
          (append (cadr le) (list-bound-vars (caddr le)))
          (if (eq? (car le) 'if)
            (append (list-bound-vars (cadr le))
                    (list-bound-vars (caddr le))
                    (list-bound-vars (cadddr le))) 
            (if (eq? (car le) 'let)
              (let ((asslst (cadr le))
                    (body (caddr le)))
                (let ((varexp (split asslst '() '())))
                  (let ((varlist (car varexp))
                        (explist (cadr varexp)))
                    (append varlist
                            (apply append (map
                                            (lambda (lexp)
                                              (list-bound-vars lexp))
                                            (cons body explist)))))))
              (if (eq? (car le) 'letrec)
                (let ((asslst (cadr le))
                      (body (caddr le)))
                  (let ((varexp (split asslst '() '())))
                    (let ((varlist (car varexp))
                          (explist (cadr varexp)))
                      (append varlist
                              (apply append (map
                                              (lambda (lexp)
                                                (list-bound-vars lexp))
                                              (cons body explist)))))))
                (if (pair? le)
                  (apply append (map
                                  (lambda (lexp)
                                    (list-bound-vars lexp))
                                  le)))))))))))

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

;;; 3.1.5>
;;; Extend the language of exercise 2.3.10 to include 'let' and 'letrec'
;;; expressions. Adapt your program lexical-address so that it recognizes
;;; 'letrec' expressions.
(define lexical-address
  (lambda (le)
    (if (pair? (free-vars le))
      (caddr (lexical-address-subst
              (list 'lambda (free-vars le) le)
              (list (free-vars le))))
      (lexical-address-subst le '()))))

(define lexical-address-subst
  (lambda (le larglst)
    (if (null? le)
      '()
      (if (symbol? le)
        (find-lexical-address le larglst 0)
        (if (eq? (car le) 'lambda)
          (list 'lambda (cadr le)
                (lexical-address-subst (caddr le)
                                       (append (list (cadr le)) larglst)))
          (if (eq? (car le) 'if)
            (list 'if
                  (lexical-address-subst (cadr le) larglst)
                  (lexical-address-subst (caddr le) larglst)
                  (lexical-address-subst (cadddr le) larglst))
            (if (eq? (car le) 'let)
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
                                                         larglst))))))
              (if (eq? (car le) 'letrec)
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
                              (lexical-address-subst body newlarglst))))))
                (if (pair? le)
                  (map (lambda (lexp)
                     (lexical-address-subst lexp larglst))
                       le))))))))))

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

;;; 3.2.1>
;;; Write boolean procedures and-proc and or-proc as variable arity procedures.

;;; IMP NOTE: The code below defines PROCEDURES, which means they
;;; evaluate all their arguments by default. The only place where
;;; I can think of using something like these is in testing a bunch
;;; of functions.
(define and-proc
  (lambda x
    (if (null? (cdr x))
      (car x)
      (if (car x)
        (apply and-proc (cdr x))
        #f))))

(define or-proc
  (lambda x
    (if (null? (cdr x))
      (car x)
      (let ((*value* (car x)))
        (if (*value*)
          *value*
          (apply or-proc (cdr x)))))))

;;; 3.3.1>
;;; Write a procedure if->cond that takes an if expression and returns the
;;; corresponding cond expression (but does not expand consequent otherwise).
;;; What does this procedure do with (if else 1 2)? In addition, write a 
;;; procedure cond->if that takes a cond expression as its argument and
;;; returns the corresponding if expression.

(define if->cond
  (lambda (ifexp)
    (letrec ((if->lop (lambda (ifexp)
                        (let ((third (cdddr ifexp)))
                          (cond
                            ((null? third) (list (list (cadr ifexp)
                                                       (caddr ifexp))))
                            ((pair? (car third))
                             (if (eq? (caar third) 'if)
                               (append (list (list (cadr ifexp)
                                                   (caddr ifexp)))
                                       (if->lop (car third)))
                               (list (list (cadr ifexp)
                                           (caddr ifexp))
                                     (list 'else
                                           (cadddr ifexp)))))
                            (else (list (list (cadr ifexp)
                                              (caddr ifexp))
                                        (list 'else
                                              (cadddr ifexp)))))))))

      (cons 'cond (if->lop ifexp)))))

(define if->lop
  (lambda (ifexp)
    (let ((third (cdddr ifexp)))
      (cond
        ((null? third) (list (list (cadr ifexp)
                                   (caddr ifexp))))
        ((pair? (car third))
         (if (eq? (caar third) 'if)
           (append (list (list (cadr ifexp)
                         (caddr ifexp)))
                   (if->lop (car third)))
           (list (list (cadr ifexp)
                       (caddr ifexp))
                 (list 'else
                       (cadddr ifexp)))))
        (else (list (list (cadr ifexp)
                          (caddr ifexp))
                    (list 'else
                          (cadddr ifexp))))))))

(define cond->if
  (lambda (condexp)
    (letrec ((ifclause (lambda (lop)
                         (cond
                           ((null? (cdr lop)) (ifexp (car lop)))
                           (else (append (ifexp (car lop))
                                         (list (ifclause (cdr lop))))))))
             (ifexp (lambda (pair)
                      (cond
                        ((null? pair) '())
                        ((eq? (car pair) 'else) (cadr pair))
                        (else (list 'if (car pair) (cadr pair)))))))
      (cond
        ((null? condexp) '())
        ((eq? (car condexp) 'cond)
         (ifclause (cdr condexp)))))))

;;; 3.4.1>
;;; Redefine leaf-sum for trees with leaf nodes.

(define-record interior (symbol left-tree right-tree))
(define-record leaf (number))

(define leaf-sum
  (lambda (tree)
    (cond
      ((leaf? tree) (leaf-number tree))
      ((interior? tree)
       (+ (leaf-sum (interior-left-tree tree))
          (leaf-sum (interior-right-tree tree))))
      (else (error 'leaf-sum
                   "Invalid tree" tree)))))

(define leaf-sum-variant
  ;;; leaf-sum with variant-case
  (lambda (tree)
    (variant-case tree
      (leaf (number) number)
        (interior (left-tree right-tree)
          (+ (leaf-sum left-tree)
          (leaf-sum right-tree))))))

;;; 3.4.2>
;;; Use variant-case to write max-interior, which takes
;;; a binary tree of numbers with at least one interior
;;; node and returns the symbol associated with an
;;; interior node with a maximal leaf sum.
;;;
;;; The program should not perform more additions than
;;; necessary, hence leaf-sum cannot be used. Instead,
;;; use an auxiliary procedure that is similar to max-interior
;;; but returns both the symbol of a maximal sum node and
;;; the value of the maximal sum. Use a new type of record
;;; to contain both the value and the sum.

(load "variant-case.scm")

(define-record interior-val (sym val))
(define-record leaf-val (val))

(define max-interior
  (lambda (bin-tree)
    'do-something))
