#lang racket
(define ns (make-base-namespace))
(provide (all-defined-out))


(define := (lambda (var value) 
    (list var value)
))


(define -- (lambda args 
  (list 'let args)
))


(define @ (lambda (bindings expr) 
  (append bindings expr) )
)


(define split_at_delim (lambda (delim args) 
  (foldr (lambda (head tail)
    (if  (eqv? head delim)
      (cons '() tail)
      (cons (cons head (car tail)) (cdr tail) ))
    ) 
   (list '()) args)))


(define split_at_delim2 (lambda (delim args) 
  (foldr (lambda (head tail)
    (if  (eqv? head delim)
      (cons '() tail)
      (cons (cons head (car tail)) (cdr tail) ))
    ) 
   '() args)))


(define parse_expr (lambda (expr)      ;expr + expr
  (cond
  [(and (list? expr) (>  (length expr) 2) (member '+ expr))        
        (cons '+ ( map term (split_at_delim  '+ expr)) )]
  [else (term expr)]
  )))


(define term (lambda (expr)            ;expr * expr
  (cond
  [(and (list? expr) (>  (length expr) 2) (member '* expr))        
        (cons '* ( map factor (split_at_delim  '* expr)) )]
  [else (factor expr)]
  )))


(define factor (lambda (expr)         ;binding list @ expression
  (cond
  [(and (list? expr) (>  (length expr) 2) (member '@ expr))
        (list 'let  (car (BindList (caar (split_at_delim  '@ expr)))) (parse_expr (cdr (member '@ expr))))]       
  [else (bindL expr)]
  )))
 
 
(define bindL (lambda (expr)         ;checks for binding list
  (cond
  [(and (list? expr) (= 1 (length expr)))        
        (parse_expr (car expr))]
  [(and (list? expr) (>  (length expr)) 1)  (parse_expr expr)]
  [else expr]
    )))


(define assignmentCheck (lambda (expr)   ;is a valid binding list
  (cond
    [(and (list? expr) (=  (length expr) 3) (eqv? (cadr expr) ':=)) 0]
    [(null? expr) 0]
    [else 1]
  )))


(define break (lambda (expr)            ;check for assignment: "var := var" or "var := number"
  (cond 
       [(number? (caddr expr))  (list (cadar expr) (caddr expr))]
       [else (list (cadar expr) (cadr (caddr expr)) ) ]
       )))


(define assignmentList (lambda (expr)  ;is an assignment? if so check which type
  (cond
    [(and (list? expr) (=  (length expr) 3) (eqv? (cadr expr) ':=))
      (break expr)      
    ]
    [(null? expr) '()] 
    [else #f] 
  )))


(define BindList (lambda (expr)       ;remove --
  (cond
    [(eq? 0 (foldl + 0 (map assignmentCheck (split_at_delim2 '-- expr))))
      (list  (map assignmentList (split_at_delim2 '-- expr)))
    ]
    [else #f]
  )))


(define eval_expr (lambda(expr)

  (eval (parse_expr expr) ns)
))
