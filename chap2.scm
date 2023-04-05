;;; This file contains all the programs given as
;;; exercises in Chapter: 2: Induction, Recursion and Scope.

;;; 2.2.5: subst using map
(define subst
  (lambda (new old slst)
    (map (keep-or-remove new old) slst)))

(define keep-or-remove
  (lambda (new old)
    (lambda (sym)
      (if (eq? sym old)
        new
        sym))))

;;; 2.2.7:
;;; 1> (duple n x) returns a list containing n copies of x
(define duple
  (lambda (n x)
    (if (< n 0)
      (display "duple: first argument should be nonnegative.")
      (if (zero? n)
        '()
        (cons x (duple (- n 1) x))))))

;;; 2> (invert lst) where lst is a list of 2-lists, returns a list with
;;; each 2-list reversed.
(define invert
  (lambda (lst)
    (if (null? lst)
      '()
      (cons (revpair (car lst)) (invert (cdr lst))))))

(define revpair
  (lambda (p)
    (list (cadr p) (car p))))

;;; 3> (list-index s los) returns the zero-based index of
;;; the first occurence of s in los, or -1 otherwise

;;; NOTE: This is the only way I could think of. There may be a much
;;; better way, however, it has escaped me. :'(
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

;;; 4> (vector-index s vos) returns the zero-based index of the first
;;; occurence of s in vos, or -1 if there is no occurence of s in vos
(define partial-vector-index
  (lambda (s vos i n)
    (if (= i n)
      0
      (if (eq? s (vector-ref vos i))
        0
        (+ 1 (partial-vector-index s vos (+ i 1) n))))))

(define vector-index
  (lambda (s vos)
    (normalize (partial-vector-index s vos 0 (vector-length vos)) (vector-length vos))))

;;; 5> (ribassoc s los v fail-value) returns the value of v that is
;;; associated with s, or fail-value if there is no associated value.
;;; If the first occurence of s in los has index n, the value associated
;;; with s is the n-th value in v. There is no associated value for s
;;; if s is not a member of los. Assume los, v have same length.
(define ribassoc
  (lambda (s los v fail-value)
    (if (eq? -1 (list-index s los))
      fail-value
      (vector-ref v (list-index s los)))))

;;; 6> (filter-in p lst), where p is a predicate, returns the list of
;;; those elements in lst that satisfy the predicate.
(define filter-in
  (lambda (p lst)
    (if (null? lst)
      '()
      (if (p (car lst))
        (cons (car lst) (filter-in p (cdr lst)))
        (filter-in p (cdr lst))))))

;;; 7> (product los1 los2) returns a list of 2-lists that represents the
;;; Cartesian product of los1 and los2.
(define product
  (lambda (los1 los2)
    (if (null? los1)
      '()
      (append (pair-up (car los1) los2)
              (product (cdr los1) los2)))))

(define pair-up
  (lambda (s los)
    (if (null? los)
      '()
      (cons (list s (car los)) (pair-up s (cdr los))))))

;;; 8> (swapper s1 s2 slst) returns a list the same as slst, but with
;;; all occurences of s1 replaced by s2 and vice versa.
(define swapper
  (lambda (s1 s2 slst)
    (if (null? slst)
      '()
      (if (eq? (car slst) s1)
        (cons s2 (swapper s1 s2 (cdr slst)))
        (if (eq? (car slst) s2)
          (cons s1 (swapper s1 s2 (cdr slst)))
          (if (pair? (car slst))
            (cons (swapper s1 s2 (car slst)) (swapper s1 s2 (cdr slst)))
            (cons (car slst) (swapper s1 s2 (cdr slst)))))))))

;;; 9> (rotate los)returns a list similar to los, except that the last
;;; element of los becomes the first in the returned list.
(define rotate-inc
  (lambda (los res)
    (if (null? los)
      '()
      (if (null? (cdr los))
        (append (list (car los)) res)
        (rotate-inc (cdr los) (append res (list (car los))))))))

(define rotate
  (lambda (los)
    (rotate-inc los '())))

;;; 2.2.8:
;;; 1> (down lst) wraps parentheses around each top-level element of lst.
(define wrap ;; This was not required, but I kept it since it's a pretty
             ;; cool function anyway!
  (lambda (lst)
    (if (null? lst)
      '()
      (if (pair? (car lst))
        (cons (down (car lst)) (down (cdr lst)))
        (cons (list (car lst)) (down (cdr lst)))))))

(define down
  (lambda (lst)
    (if (null? lst)
      '()
      (cons (list (car lst)) (down (cdr lst))))))

;;; 2> (up lst) removes a pair of parentheses from each top-level element
;;; of lst. If a top-level element is not a list it is included in the
;;; result as-is.
(define up
  (lambda (lst)
    (if (null? lst)
      '()
      (if (pair? (car lst))
        (append (car lst) (up (cdr lst)))
        (cons (car lst) (up (cdr lst)))))))

;;; 3> (count-occurences s slst) returns the number of occurence of s in slst
(define count-occurences
  (lambda (s slst)
    (if (null? slst)
      0
      (if (symbol? (car slst))
        (if (eq? s (car slst))
          (+ 1 (count-occurences s (cdr slst)))
          (count-occurences s (cdr slst)))
        (+ (count-occurences s (car slst))
           (count-occurences s (cdr slst)))))))

;;; 4> (flatten slst) returns a list of the symbols contained in
;;; slst in the order in which they occur when slst is printed.
(define flatten
  (lambda (slst)
    (if (null? slst)
      '()
      (if (symbol? (car slst))
        (cons (car slst) (flatten (cdr slst)))
        (append (flatten (car slst)) (flatten (cdr slst)))))))

;;; 5> (merge lon1 lon2), where lon1 and lon2 are lists of numbers that are
;;; sorted in ascending order, returns a sorted list of all the numbers
;;; in lon1 and lon2.
(define merge
  (lambda (lon1 lon2)
    (if (null? lon1)
      lon2
      (if (null? lon2)
        lon1
        (if (<= (car lon1) (car lon2))
          (cons (car lon1)
                (merge (cdr lon1) lon2))
          (cons (car lon2)
                (merge lon1 (cdr lon2))))))))

;;; 2.2.9:
;;; 1> (path n bst), where n is a number an bst is a binary search tree
;;; that contains the number n, returns a list of Ls and Rs showing how
;;; to find the node containing n. If n is found at the root, it returns
;;; the empty list.
(define path
  (lambda (n bst)
    (if (eq? n (car bst))
      '()
      (if (> n (car bst))
        (cons 'R (path n (caddr bst)))
        (cons 'L (path n (cadr bst)))))))

;;; 2> (car&cdr s slst errvalue) returns an expression that, when evaluated,
;;; produces the code for a procedure that takes a list with the same
;;; structure as slst and returns the value in the same position as the
;;; leftmost occurence of s in slst. If s does not occur in slst, then
;;; errvalue is returned.
(define car&cdr
  (lambda (s slst errvalue)
    (if (null? slst)
      errvalue
      (if (null? (traceroute s slst 'lst))
        errvalue
        (list 'lambda '(lst) (traceroute s slst 'lst))))))

(define traceroute
  (lambda (s slst sym)
    (if (null? slst)
      '()
      (if (symbol? (car slst))
        (if (eq? s (car slst))
          (list 'car sym)
          (traceroute s (cdr slst) (list 'cdr sym)))
        (if (null? (traceroute s (car slst) sym))
          (traceroute s (cdr slst) (list 'cdr sym))
          (traceroute s (car slst) (list 'car sym)))))))

;;; 3> (car&cdr2 s slst errvalue) : same as above, but it generates
;;; procedure compositions
(define car&cdr2
  (lambda (s slst errvalue)
    (if (null? slst)
      errvalue
      (if (null? (traceroute2 s slst '()))
        errvalue
        (traceroute2 s slst '())))))

(define traceroute2
  (lambda (s slst sym)
    (if (null? slst)
      '()
      (if (symbol? (car slst))
        (if (eq? s (car slst))
          (if (null? sym)
            'car
            (list 'compose 'car sym))
          (if (null? sym)
            (traceroute2 s (cdr slst) 'cdr)
            (traceroute2 s (cdr slst) (list 'compose 'cdr sym))))
        (if (null? (traceroute2 s (car slst) sym))
          (if (null? sym)
            (traceroute2 s (cdr slst) 'cdr)
            (traceroute2 s (cdr slst) (list 'compose 'cdr sym)))
          (if (null? sym)
            (traceroute2 s (car slst) 'car)
            (traceroute2 s (car slst) (list 'compose 'car sym))))))))

;;; 4> (compose p1 ... pn), where p1, ..., pn is a sequence of zero or
;;; more procedures of one argument. returns the composition of all the
;;; procedures. The composition of zero procedures is the identity procedure.
;;; The composition of one procedure is the procedure itself, and the
;;; composition of two or more procedures is given by:
;;; ((compose p1 p2 ...) x) => (p1 ((compose p2 ...) x))
(define compose
  (lambda p
    (if (null? p)
      (lambda (x) x)
      (comp (car p) (apply compose (cdr p))))))

(define comp
  (lambda (p1 p2)
    (lambda (x)
      (p1 (p2 x)))))

;;; 5> (sort lon) returns a list of the elements of lon in increasing order.
(define sort
  (lambda (lon)
    (if (null? lon)
      '()
      (insert (car lon) (sort (cdr lon))))))

(define insert
  (lambda (n lon)
    (if (null? lon)
      (list n)
      (if (<= n (car lon))
        (cons n lon)
        (cons (car lon) (insert n (cdr lon)))))))

;;; 6> (sort2 predicate lon) returns a list of elements determined by
;;; the predicate
(define sort2
  (lambda (predicate lon)
    (if (null? lon)
      '()
      (insert2 predicate (car lon) (sort2 predicate (cdr lon))))))

(define insert2
  (lambda (p n lon)
    (if (null? lon)
      (list n)
      (if (p n (car lon))
        (cons n lon)
        (cons (car lon) (insert2 p n (cdr lon)))))))

;;; 2.3.1:
;;; free-vars takes a list structure representing an expression in the lambda
;;; calculus syntax and returns a set (a list without duplicates) of all
;;; the variables that occur free in the expression. Similarly for 
;;; bound-vars
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
            (if (pair? le)
              (apply append (map 
                              (lambda (lexp) 
                                (list-free-vars lexp los))
                              le)))))))))

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
            (if (pair? le)
              (apply append (map
                              (lambda (lexp)
                                (list-bound-vars lexp))
                              le)))))))))

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

;;; 2.3.2:
;;; Write predicates free? and bound? that determine if an expression
;;; contains a free (resp. bound) occurence of a particular variable.
(define free?
  (lambda (var le)
    (belongs var (free-vars le))))

(define bound?
  (lambda (var le)
    (belongs var (bound-vars le))))

;;; 2.3.10:
;;; Consider the subset of Scheme given by:
;;; <exp> := <varref>
;;;       | (if <exp> <exp> <exp>)
;;;       | (lambda ({<var>}*) <exp>)
;;;       | ({<exp>}+)
;;; Write procedure lexical-address that takes any expression and
;;; returns the expression with every variable reference v replaced
;;; by a list (v : d p), as above. If the variable refers to a free
;;; variable, imagine the entire expression is wrapped within a lambda
;;; that binds it.
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
            (if (pair? le)
              (map (lambda (lexp)
                     (lexical-address-subst lexp larglst))
                   le))))))))

(define find-lexical-address
  (lambda (sym larglst d)
    (if (eq? -1 (list-index sym (car larglst)))
      (find-lexical-address sym (cdr larglst) (+ d 1))
      (list sym ': d (list-index sym (car larglst))))))

;;; 2.3.13>
;;; Write procedure un-lexical-address, which takes lexical-address
;;; expressions with formal parameter lists and with variable references
;;; of the form (: d p), and returns and equivalent expression formed by
;;; substituting standard variable references for the lexical address
;;; information, or #f if no such expression exists.
(define un-lexical-address
  (lambda (lae)
    (if (null? lae)
      '()
      (if (belongs-rec #f (un-lexical-address-subst lae '()))
        #f
        (un-lexical-address-subst lae '())))))

(define un-lexical-address-subst
  (lambda (lae larglst)
    (if (null? lae)
      '()
      (if (eq? (car lae) ':)
        (find-variable (cadr lae) (caddr lae) larglst)
        (if (eq? (car lae) 'lambda)
          (list 'lambda (cadr lae)
                (un-lexical-address-subst (caddr lae)
                                          (append (list (cadr lae)) larglst)))
          (if (eq? (car lae) 'if)
            (list 'if
                  (un-lexical-address-subst (cadr lae) larglst)
                  (un-lexical-address-subst (caddr lae) larglst)
                  (un-lexical-address-subst (cadddr lae) larglst))
            (if (pair? lae)
              (map (lambda (lexp)
                     (un-lexical-address-subst lexp larglst))
                   lae))))))))

(define find-variable
  (lambda (d p larglist)
    (if (verify (list-ref (list-ref larglist d) p)
                (list-initial d larglist))
      (list-ref (list-ref larglist d) p)
      #f)))

(define verify
  (lambda (sym larglst)
    (if (null? larglst)
      #t
      (if (belongs sym (car larglst))
        #f
        (verify sym (cdr larglst))))))

(define list-initial
  (lambda (n lst)
    (if (zero? n)
      '()
      (if (null? lst)
        '()
        (cons (car lst) (list-initial (- n 1) (cdr lst)))))))

(define belongs-rec
  (lambda (sym slst)
    (if (null? slst)
      #f
      (if (pair? slst)
        (if (eq? (car slst) sym)
          #t
          (if (pair? (car slst))
            (or (belongs-rec sym (car slst))
                (belongs-rec sym (cdr slst)))
            (belongs-rec sym (cdr slst))))
        (if (eq? slst sym)
          #t
          )))))

;;; 2.3.14>
;;; Write procedure rename that takes a lambda calculus expression exp
;;; and two variables var1 and var2 and returns exp[var1/var2] if var1 does
;;; not occur free in exp and #f otherwise.
(define renaming
  (lambda (exp var1 var2)
    (if (null? exp)
      '()
      (if (belongs var1 (free-vars exp))
        #f
        (if (symbol? exp)
          (if (eq? var2 exp)
            var1
            exp)
          (if (eq? (car exp) 'lambda)
            (list 'lambda (subst var1 var2 (cadr exp))
                  (renaming (caddr exp) var1 var2))
            (if (pair? exp)
              (list (renaming (car exp) var1 var2)
                      (renaming (cadr exp) var1 var2)))))))))

(define rename
  (lambda (exp var1 var2)
    (if (null? exp)
      '()
      (if (belongs-rec #f (renaming exp var1 var2))
        #f
        (renaming exp var1 var2)))))

;;; 2.3.15>
;;; Use rename to write a procedure alpha-convert that takes a lambda
;;; expression exp of the form (lambda (var) exp), and a variable v,
;;; and returns (lambda (v) exp[v/var]), or #f if v occurs free in exp.
(define alpha-conversion
  (lambda (exp v)
    (list 'lambda (subst v (caadr exp) (cadr exp))
          (rename (caddr exp) v (caadr exp)))))

(define alpha-convert
  (lambda (exp v)
    (if (belongs-rec #f (alpha-conversion exp v))
      #f
      (alpha-conversion exp v))))
