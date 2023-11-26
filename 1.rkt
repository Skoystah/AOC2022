#lang r7rs

(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme cxr))

(define (elf)
  (call-with-input-file "1-input.txt"
    (lambda (in)
      (let loop ((line (read-line in))
                 (result '())
                 (cur-elf 0))
        (cond 
          ((eof-object? line) (reverse (insert-sort cur-elf result)))
          ((string->number line) (loop (read-line in) 
                                        result
                                       (+ cur-elf (string->number line))))
          (else (loop (read-line in)(insert-sort cur-elf result) 0)))))))

(define (insert-sort el lst)
  (cond
    ((null? lst) (list el))
    ((< el (car lst)) (cons el lst))
    (else (cons (car lst)
                (insert-sort el (cdr lst))))))
       
(define (accumulate lst op ne)
  (if (null? lst)
      ne
      (op (car lst)
          (accumulate (cdr lst) op ne))))

                     
(define (list-max lst)
  (let loop ((m 0)
             (l lst))
    (cond
      ((null? l) m)
      ((> (car l) m) (loop (car l)
                           (cdr l)))
      (else (loop m (cdr l))))))

(display (+ (car (elf))(cadr (elf))(caddr (elf))))
