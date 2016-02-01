;5 3 0 0 7 0 0 0 0
;6 0 0 1 9 5 0 0 0
;0 9 8 0 0 0 0 6 0
;8 0 0 0 6 0 0 0 3
;4 0 0 8 0 3 0 0 1
;7 0 0 0 2 0 0 0 6
;0 6 0 0 0 0 2 8 0
;0 0 0 4 1 9 0 0 5
;0 0 0 0 8 0 0 7 9

(define boards
  (list
   '((3 0 0  2 4 0  0 6 0)
     (0 4 0  0 0 0  0 5 3)
     (1 8 9  6 3 5  4 0 0)
      
     (0 0 0  0 8 0  2 0 0)
     (0 0 7  4 9 6  8 0 1)
     (8 9 3  1 5 0  6 0 4)
     
     (0 0 1  9 2 0  5 0 0)
     (2 0 0  3 0 0  7 4 0)
     (9 6 0  5 0 0  3 0 2))
    
  '((5 3 0  0 7 0  0 0 0)
    (6 0 0  1 9 5  0 0 0)
    (0 9 8  0 0 0  0 6 0)
   
    (8 0 0  0 6 0  0 0 3)
    (4 0 0  8 0 3  0 0 1)
    (7 0 0  0 2 0  0 0 6)
   
    (0 6 0  0 0 0  2 8 0)
    (0 0 0  4 1 9  0 0 5)
    (0 0 0  0 8 0  0 7 9))
   
 '((2 0 0  8 0 4  0 0 6)
   (0 0 6  0 0 0  5 0 0)
   (0 7 4  0 0 0  9 2 0)
   
   (3 0 0  0 4 0  0 0 7)
   (0 0 0  3 0 5  0 0 0)
   (4 0 0  0 6 0  0 0 9)
   
   (0 1 9  0 0 0  7 4 0)
   (0 0 8  0 0 0  2 0 0)
   (5 0 0  6 0 8  0 0 1))
  
 '((7 8 0  0 0 0  0 4 6)
   (0 0 6  3 0 0  0 0 0)
   (1 0 0  0 8 9  0 0 0)
   
   (0 7 5  0 0 0  8 1 0)
   (0 0 0  4 0 5  0 0 0)
   (0 9 1  0 0 0  4 3 0)
   
   (0 0 0  8 3 0  0 0 4)
   (0 0 0  0 0 4  6 0 0)
   (8 3 0  0 0 0  0 5 1))
  )
)

(define (get-column i m)
  (map (lambda(row)(list-ref row i)) m)
)

(define (get-row i m)
  (list-ref m i)
)

(define (getj j lst)(list-ref lst j))

(define (get-elem i j m)
   (getj j (get-row i m))
)

(define (accumulate op null-value term a next b)
  (if (> a b)
      null-value
      (op (term a) (accumulate op null-value term (next a) next b))))

(define (next k)(+ k 1))
(define (transpose m)
  (accumulate cons '() (lambda (i) (get-column i m)) 0 next 8))

(define (elem x lst)
 (cond((null? lst) #f)
      ((= x (car lst)) #t)
      (else (elem x (cdr lst)))
 )
)

;permutes the the numbers from 0 to p-1 in a list
(define (permute res p)
  (define (helper res k)
    (define rand_num (random p))
    (cond((= (length res) p) res)
         ((not(elem rand_num res))(if(even? k)(helper (reverse (cons rand_num (reverse res)))(+ k 1))
                                  (helper (cons rand_num (reverse res)) (+ k 1)))
         )
         (else (helper res (+ k 1)))
    )
  )(helper res 0)
)

;(permute '() 9)

;changes the places of the first and the third horizontal division
(define (changeHorizontalDivision m)
   (list (get-row 6 m) (get-row 7 m) (get-row 8 m) (get-row 3 m) (get-row 4 m) (get-row 5 m) (get-row 0 m) (get-row 1 m) (get-row 2 m))
)

;changes the places of rows in one horizontal division
(define (changeRowsInHor m)
  (define permuted (permute '() 3))
   (define (helper permuted res)
      (cond((null? permuted) res)
           (else (helper (cdr permuted)(cons (get-row (car permuted) m) res)))
      )
  )(helper permuted '())
)

;takes n elements from the begining of a list
(define (get-n-items lst n)
        (if (> n 0)
            (cons (car lst) (get-n-items (cdr lst) (- n 1)))
            '()
        )
)

;takes count elements (start,..)
(define (slice lst start count)
       (if (>= start 1)
            (slice (cdr lst) (- start 1) count)
            (get-n-items lst count)
       )
)

;changes the places of rows in the three horizontal divisions
(define (horizontalTransform m)
  (append (changeRowsInHor (slice m 0 3)) (changeRowsInHor (slice m 3 3)) (changeRowsInHor (slice m 6 3)))   
)

;changes the places of columns in the three vertical divisions
(define (verticalTransform m)
   (let ((transposed (transpose m)))
      (transpose (horizontalTransform transposed))
   )
)

(define (symetricSecondDiag m)
     (reverse (transpose (reverse m)))
)

(define (otherTransform m)
   (define other (list "main diagonal" "second diagonal"  
                  "change first and last horizontal division" "horizontal" "vertical"))
   (define type (list-ref other (random 5)))
   (cond
        ((eq? type "main diagonal")(transpose m))
        ((eq? type "second diagonal")(symetricSecondDiag m))
        ((eq? type "change first and last horizontal division")(changeHorizontalDivision m))
        ((eq? type "horizontal")(horizontalTransform m))
        ((eq? type "vertical")(verticalTransform m))
   )
)

(define (transformBoard m type)
  (cond((eq? type "horizontal")(horizontalTransform m))
       ((eq? type "vertical")(verticalTransform m))
       ((eq? type "other")(otherTransform m))
  )
)

(define (generateNewBoard boards)
  (define options (list "horizontal" "vertical" "other"))
  (define option (list-ref options (random 3)))
  (transformBoard (list-ref boards (random 4)) option)
)

(define (colChecker num col m)
  (define (helper i)
    (cond((>= i 9) #t)
          ((= (get-elem i col m) num) #f)
          (else (helper (+ i 1)))
    )
  )(helper 0)
)

(define (rowChecker num row m)
  (define (helper j)
    (cond ((>= j 9) #t)
          ((= (get-elem row j m) num) #f)
          (else (helper (+ j 1)))
    )
  )(helper 0)
)

(define (listOf s)
  (apply append s)
)

(define (squareCheckHelper row col m)
    (let*
         (
          (startRow (* 3 (quotient row 3)))
          (startCol (* 3 (quotient col 3)))
          (finalRow (+ startRow 3))
          (finalCol (+ startCol 3))
          (withNewCols
           (map (lambda(myrow)(slice myrow startCol (- finalCol startCol) )) m)
           )
          (square
           (slice withNewCols startRow (- finalRow startRow))
          )
         )
   (listOf square)
   )
)


(define (squareChecker num row col m)
  (define listOfSquare (squareCheckHelper row col m))
  (cond((elem num listOfSquare)#f)
       (else #t)
  )
)

(define (isPossible num row col m)
  (let*
      ((shiftRow (* 3 (quotient row 3)))
      (shiftCol (* 3 (quotient col 3))))
       (and (rowChecker num row m) (colChecker num col m)
       (squareChecker num shiftRow shiftCol m)(= (get-elem row col m) 0))
  )
)

(define (replace lst el n)
  (cond
    ((null? lst) '())
    ((= n 0) (cons el (cdr lst)))
    (else (cons (car lst) (replace (cdr lst) el (- n 1))))))

(define (replaceInBoard m el k n)
  (cond((null? m) '())
        ((= k 0)(cons (replace (get-row k m) el n)(cdr m)))
        (else (cons (car m)(replaceInBoard (cdr m) el (- k 1) n)))
   )
)

(define (possibleNumbers row col m)
  (define (helper num res)
      (cond((> num 9) res)
           ((isPossible num row col m)(cons num (helper (+ num 1) res)))
           (else (helper (+ num 1) res))
      )
  )(helper 1 '())
)

(define (findUnassignedCell m row col)
  (define (helper i j)
     (cond((= (get-elem i j m) 0)(list i j))
          ((< j 8)(helper i (+ j 1)))
          ((and (= j 8)(< i 8))(helper (+ i 1) 0))
          ((and(= i 8)(= j 8)) (list "end" "end"))
     )
  )(helper row col)
)
 
  (define (iterateBoard row col m)
    (let*
        ((firstUnassigned (findUnassignedCell m row col))
        (rowUnassigned (car firstUnassigned))
        (colUnassigned (cadr firstUnassigned)))
      (cond ((and (eq? rowUnassigned "end")(eq? colUnassigned "end")) m)
            (else (attemptToAssign (possibleNumbers rowUnassigned colUnassigned m) rowUnassigned colUnassigned m))
      )
    )
  )

(define (attemptToAssign possibleList row col m)
  (cond ((null? possibleList) '());ot upr po R
        (else (iteratePossibleList possibleList row col m))
  )
)

 (define (iteratePossibleList possibleList row col m)
    (let ((checker (assignNum row col (car possibleList) m)))
      (cond ((null? checker)(attemptToAssign (cdr possibleList) row col m));backtracking
            (else checker)
      )
    )
  )
  
  (define  (assignNum row col num m)
        (iterateBoard row col (replaceInBoard m num row col))
  )
  
 (define (readRow count res)
  (cond((>= count 9)(reverse res))
        (else (begin
               (define row (read))
               (readRow (+ count 1) (cons row res))
               )               
        )
  )
)
 
(define (readBoard count res)
   (cond((>= count 9)(begin                 
                     (display "Решение:\n")
                     (displayFormatted (iterateBoard 0 0 (reverse res)))
                     ))
        (else (begin
               (readBoard (+ count 1)(cons (readRow 0 '()) res)))
         )
   )
)

(define (displayFormatted m)
 (define (helper lst)
  (cond((not(null? lst))(begin
                       (display (car lst))
                       (display "\n")
                       (helper (cdr lst)) 
                       )
       )
       (else (display "\n"))
  )
 )(helper m)
)

(define (Choose option)
(cond((eq? option 'autoboard)(begin
                                 (display "Автоматично генерирана дъска:\n")
                                 (define newboard (generateNewBoard boards))
                                 (displayFormatted newboard)
                                 (display "Решение:\n")
                                 (displayFormatted (iterateBoard 0 0 newboard))
                                 ))
        (else (begin
              (display "Moля, въведете дъска. Отделяйте въведените цифри с интервал, като празните позиции означите с 0.
Въвеждането на ред се извършва след натискането на клавиша Enter.\n")
              (readBoard 0 '())
              )
       )
  )
)

(display "Ако желаете автоматично генериранe на дъска, моля в долния прозорец напишете `autoboard`.
За да въведете своя дъска, моля напишете `myboard`. \n")
(define chosen (read))
(Choose chosen)







