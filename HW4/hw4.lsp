; Jinbean Park - 805330751
; Programming Assignment 4

;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists


; 1. Arguments: (1) clause is the list that is the clause of the CNF. (list)
; (2) literal is the number that is the literal in the clause. (number)
; 2. Return value: the function removeNlit returns a clause removed the literal. (list)
; 3. Explaining the method removeNlit (clause literal): The program is able to remove literal 
; in a clause through the function removeNlit.
(defun removeNlit (clause literal)

  (cond
    ; Case 1: Reaching to end of the clause after removing literal.
    ((NULL clause) NIL)
    ; Case 2: Encountering the literal and then removing it.
    ((equal (car clause) literal) (cdr clause))
    ; Case 3: The first element of clause is not literal.
    (t (cons (car clause) (removeNlit (cdr clause) literal)))
  )
)


; 1. Arguments: (1) delta represents the CNF (list).
; (2) literal is the number that is the literal in the clause. (number)
; 2. Return value: the function updateDelta returns a updated CNF. (list)
; 3. Explaining the method updateDelta (delta literal): If the program finds the literal,
; the program removes the clause containing the literal because the clause containing the
; literal is always true no matter what other literals are in the clause. In contrast,
; the program removes only the -literal from the clause if the clause contains -literal.
(defun updateDelta (delta literal)
  
  (cond
    ; Case 1: Finishing checking every clause in delta.
    ((NULL delta) NIL)
    ; Case 2: If clause has a literal, the program removes that clause from delta
    ; because the clause having the literal is always true no matter what other literals are.
    ((member literal (first delta)) (updateDelta (cdr delta) literal))
    (t
      (cond
        ; Case 3: If clause has a -literal, it removes only -literal from the clause.
        ((member (* -1 literal) (first delta)) (cons (removeNlit (car delta) (* -1 literal)) (updateDelta (cdr delta) literal)))
        ; Case 4: If clause doesn't have both literal and -literal, it just moves to next clause.
        (t (cons (car delta) (updateDelta (cdr delta) literal)))
      )
    )
  )
)


; 1. Arguments: (1) delta represents the CNF (list).
; (2) literal is the number that is the literal in the clause. (number)
; 2. Return value: chkUnSAT returns NIL if it couldn't find a clause that is equal to
; the (-literal). However, chkUnSAT returns t if it finds a clause that is equal to
; the (-literal). (boolean)
; 3. Explaining the method chkUnSAT (delta literal): The program is able to check whether
; the delta contains (-literal) or not. If the delta contains (-literal), it means that
; the clause having (-literal) is always false, and it makes CNF always be False no matter what
; other clauses are. Therefore, the function should return t and make getNewDelta function return
; UnSat so that the program finally returns NIL.
(defun chkUnSAT (delta literal)

  (cond
    ; Case 1: The program couldn't find (-literal) in delta, and then returns NIL
    ((NULL delta) NIL)
    ; Case 2: The program finds (-literal) in delta, and then returns t.
    ((equal (list (* -1 literal)) (first delta)) t)
    ; Case 3: The first clause is different to (-literal).
    (t (chkUnSAT (cdr delta) literal))
  )
)


; 1. Arguments: (1) delta represents the CNF (list).
; (2) literal is the number that is the literal in the clause. (number)
; 2. Return value: getNewDelta returns 'UnSat if it finds a clause that is equal to
; the (-literal). However, getNewDelta returns updated Delta if it doesn't finds a clause 
; that is equal to the (-literal). (list or atom)
; 3. Explaining the method getNewDelta (delta literal): The program is able to update delta
; corresponding to literal. If the delta contains (-literal), the function getNewDelta returns
; UnSat while it returns updated delta if it doesn't find a clause that is equal to (-literal)
; in delta.
(defun getNewDelta (delta literal)
  (cond
    ; Case 1: delta contains (-literal)
    ((chkUnSAT delta literal) 'UnSat)
    ; Case 2: delta does not contain (-literal), then it returns updated delta.
    (t (updateDelta delta literal))
  )
)


; 1. Arguments: (1) n represents the number of variables of the CNF (number).
; (2) model is a solution to the CSP that corresponds to a model of the CNF (list).
; 2. Return value: setTruetoLeft returns a model of the CNF where variables which are not
; there in a model are appended as true. (list)
; 3. Explaining the method setTrueToLeft (n model): The program is able to append variables
; which are not there in a model to the model as true.
(defun setTrueToLeft (n model)

  (cond
    ; Case 1: The number of variable is equal to the length of model.
    ((equal n (length model)) model)
    ; Case 2: Variables which are not there in a model are appended to the model as true.
    (t (setTrueToLeft n (cons (+ 1 (length model)) model)))
  )
)


; 1. Arguments: (1) n represents the number of variables of the CNF (number).
; (2) delta represents the CNF (list).
; (3) model is a solution to the CSP that corresponds to a model of the CNF (list).
; 2. Return value: findModel returns a solution to the CSP that corresponds to a model
; of the delta. (list)
; 3. Explaining the method findModel (n delta model): The program checks and appends
; the literal from 1 to n. If the new delta updated by several literals is null, it means
; that the program does not necessarily check out SAT for other literals because it is always
; true no matter what other literals are, so the program will return a model after appending
; variables which are not there in the model if some variables are missing in the model.
; However, the program will return NIL if the program encounters the unsatisfiable case.
(defun findModel (n delta model)

  (cond
    ; Case 1: The program encounters the unsatisfiable case.
    ((equal delta 'UnSat) NIL)
    ; Case 2: The program returns a model after appending variables which are not there
    ; in the model if some variables are missing in the model.
    ((NULL delta) (setTrueToLeft n model))
    ; The program checks and appends the literal from 1 to n.
    (t (let* ((posLit (+ 1 (length model)))
          (negLit (* -1 posLit))
          ; The program checks and finds updated CNF for both positive and negative literals.
          (newPosDelta (getNewDelta delta posLit))
          (newNegDelta (getNewDelta delta negLit)))        
      (cond
        ; Case 3: The program does not necessarily check out SAT for other literals 
        ; because it is always true no matter what other literals are
        ((NULL newPosDelta) (findModel n newPosDelta (cons posLit model)))
        ((NULL newNegDelta) (findModel n newNegDelta (cons negLit model)))
        ; Case 4: The program will return a solution to the CSP for either positive literal or
        ; negative literal if there exists a solution to the CSP. Otherwise, it returns NIL.
        (t (OR (findModel n newPosDelta (cons posLit model))
                (findModel n newNegDelta (cons negLit model))))
      )
      )
    )
  )
)


; 1. Arguments: (1) n represents the number of variables of the CNF (number).
; (2) delta represents the CNF (list).
; 2. Return value: Returns a list of n integers, representing a model of delta, otherwise 
; it returns NIL.
(defun sat? (n delta)
  ; Pass the argument the number of variables(=n), CNF(=delta), and promising model.
  (findModel n delta NIL)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

