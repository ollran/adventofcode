;;; Day 1: The Tyranny of the Rocket Equation

(import (chicken io))

(define input
  (let* ((in (open-input-file "input.txt"))
         (values (read-list in)))
    (close-input-port in)
    values))

(define (process-input fn input)
  (foldl (lambda (acc cur)
           (+ acc (fn cur)))
         0
         input))

(define (calculate-fuel n)
  (- (truncate (/ n 3)) 2))

(define (calculate-total-fuel n)
  (letrec ((loop (lambda (currentMass fuel)
                   (if (<= currentMass 0)
                       fuel
                       (loop (calculate-fuel currentMass)
                             (+ fuel currentMass))))))
    (loop (calculate-fuel n) 0)))

(define part01
  (process-input calculate-fuel input))

(define part02
  (process-input calculate-total-fuel input))

(print part01)
(print part02)
