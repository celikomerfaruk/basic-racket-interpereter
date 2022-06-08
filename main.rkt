; omer faruk celik

#lang racket

(provide (all-defined-out))

(define := (lambda (var value) (list var value)))

(define -- (lambda args (list 'let args)))

(define @ (lambda (bindings expr) (append bindings expr)))

(define split_at_delim (lambda (delim args)
  (foldr (lambda (cur next)
           (if (eqv? cur delim)
               (cons '() next)
               (cons (cons cur (car next)) (cdr next))))
         (list '()) args)))

(define parse_expr (lambda (expr)
  (if (list? expr)
      (if (null? (cdr expr))
          (parse_expr (car expr))
          (if (eqv? (cadr expr) ':=) 
             (if(> (length expr) 4)
                 (list 'let (assign_list expr))
                 (-- (:= (eval(car expr)) (eval(caddr expr)))))
              (if (eqv? (cadr expr) '@)
                  (@ (parse_expr (car expr))
                     (list(parse_expr (cddr expr))))
                  (if (and (eqv? (cadr expr) '*) (> (length expr) 3))
                      (if(eqv? (fourth expr) '+)
                      (list (parse_expr (fourth expr))    
                         (list(cadr expr)
                        (parse_expr (car expr))
                        (parse_expr (third expr)))
                         (parse_expr (fifth expr))
                        )
                      (list (cadr expr)
                        (parse_expr (car expr))
                        (parse_expr (cddr expr)))
                      )
                      (list (cadr expr)
                        (parse_expr (car expr))
                        (parse_expr (cddr expr)))))))
      expr)))


(define eval_expr (lambda (expr)
                    (eval(parse_expr expr))))


(define (assign_list lst)
  (if(> (length lst) 4)
                  (append (list (:= (eval(car lst)) (eval(caddr lst)) ))  (assign_list (cddddr lst)))
                  (list(:= (eval(car lst)) (eval(caddr lst))))))
