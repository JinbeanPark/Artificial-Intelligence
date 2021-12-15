; Jinbean Park - 805330751
; Programming Assignment 3


; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
)

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.


; 1: goal-test (s).
; 1. Arguments: (1) s is a goal state of the game (list).
; 2. Return value: The program will implement goal-test method recursively if box and keepr do not exist in a state s, and then it will return true after checking out every elements in a state s. On the contrary the program will return false if the program find at least a box or keeper in a state s (return boolean).
; 3. Explaining the method goal-test: The program is able to check whether the state s is goal-state or not by checking row by row. If the count value of box or keepr is greater than 0,
; it means that there exists at least a box or keepr in a state s, so the program will return NIL
; as soon as the program encounters a box or keeper.


(defun goal-test (s)

	(cond
		; Case 1: The program couldn't find a box or keeper in a state s.
		((NULL s) t)
		(t (cond
				; Case 2: The program finds at least one box or keeper in a state s.
				((OR (< 0 (count box (car s))) (< 0 (count keeper (car s)))) NIL)
				; Case 3: The program couldn't find a box or keeper in a row in state s.
				(t (goal-test (cdr s)))
		))
	)
)



; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.



; 2: get-square (s rcSqu).
; 1. Arguments: (1) s is a state of the game (list). (2) rcSqu is a position (row column) (list).
; 2. Return value: The program will return the integer content of staet S at square (r, c) (list).
; if the square is outside the scope of the problem, it will return the value of a wall.
; 3. Explaining the method get-square (s rcSqu): The program is able to return the content of state S at square (r, c) by implementing the method get-square. The method get-square returns wall if the s is null or the position of square is outside the scope of the problem, and the program checks out the elements in a row and then implements get-square recursively until 
; the value of r becomes 0. After reaching to the value of row 0, the program returns the content
; of state S at square (r, c) by using the elt.


(defun get-square (s rcSqu)

	(cond
		; Case 1: s is null
		((NULL s) wall)
		; Case 2: The square is outside the scope of the problem.
		((OR (<= (length s) (first rcSqu)) (< (first rcSqu) 0)) wall)
		((OR (<= (length (car s)) (second rcSqu)) (< (second rcSqu) 0)) wall)
		; Case 3: The program reaches to the value of row 0, and then returns the 
		; square (r, c). 
		((equal 0 (first rcSqu)) (elt (first s) (second rcSqu)))
		; Case 4: The program moves to next row recursively until reaching to the value of row 0.
		(t (get-square (cdr s) (list (- (first rcSqu) 1) (second rcSqu))))
	)
)


; 3: findRowSetV (row v lenCol). <= Helper function of set-square (s rcSqu v).
; 1. Arguments: (1) row is a one of rows in a state s (list). (2) v is a square content v to 
; replace (number) (3) lenCol is a length of column of a state s (number).
; 2. Return value: The program will return the updated row where the value at (r, c) is replaced
; with v. (list)
; 3. Explaining the method findRowSetV (row v lenCol): The program is able to update the value
; at (r, c) to the value v by using the function findRowSetV. Thus, the method findRowSetV is
; a helper function of set-square(s rcSqu v)


(defun findRowSetV (row v lenCol) 

	(cond
		; Case 1: Replace the value at (r, c) with the value v.
		((equal lenCol 0) (cons v (cdr row)))
		; Case 2: Keep the value of row until reaching to the value at (r, c).
		(t (cons (car row) (findRowSetV (cdr row) v (- lenCol 1))))
	)
)


; 4: set-square (s rcSqu v).
; 1. Arguments: (1) s is a state of the game (list). (2) rcSqu is a position (row column) (list).
; (3) v is a square content v to replace (number)
; 2. Return value: The function set-square returns a new state s' that is obtained by setting the
; square (r, c) to value v.
; 3. Explaining the method findRowSetV (row v lenCol): The program is able to return the updated
; state s' where the value at (r, c) is updated to the value v. The function uses the helper 
; function findRowSetV to check out and set the value row by row. 

(defun set-square (s rcSqu v)

	(cond
		; Case 1: The program reaches to the row r, so it calls the helper function findRowSetV
		; to update the value at (r,c) to the value v.
		((equal 0 (first rcSqu))
				(let ((rowSetV (findRowSetV (car s) v (second rcSqu))))
					(cons rowSetV (cdr s))))
		; Case 2: The program moves to the next row until reaching to the location of row r.
		(t (cons (car s) (set-square (cdr s) (list (- (first rcSqu) 1) (second rcSqu)) v)))
	)
)


; 5: handleFromKeeToBlank (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; (2) direction is a direction to move from the keeper position. (symbol).
; (3) keeperPos is a position of keeper in a state s. (r c) (list)
; (4) keeperRowPos is a row position of keeper. r (number)
; (5) keeperColPos is a column position of keeper. c (number)
; (6) keeperOrKeeStar is a content either keeper or keeperstar. (number)
; 2. Return value: Return a updated state s'. (list)
; 3. Explaning the method handleFromKeeToBlank: The program is able to handle the case that 
; keeper moves to the blank through the method handleFromKeeToBlank.


(defun handleFromKeeToBlank (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar)


	(cond
		((equal direction 'UP)
			(cond
				; Case 1. Move from keeperstart to blank
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list (- keeperRowPos 1) keeperColPos) keeper))
				; Case 2. Move from keepr to blank.
				(t (set-square (set-square s keeperPos blank) (list (- keeperRowPos 1) keeperColPos) keeper))
		))
		((equal direction 'DOWN)
			(cond
				; Case 1. Move from keeperstart to blank
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list (+ keeperRowPos 1) keeperColPos) keeper))
				; Case 2. Move from keepr to blank.
				(t (set-square (set-square s keeperPos blank) (list (+ keeperRowPos 1) keeperColPos) keeper))
		))
		((equal direction 'LEFT)
			(cond
				; Case 1. Move from keeperstart to blank
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list keeperRowPos (- keeperColPos 1)) keeper))
				; Case 2. Move from keepr to blank.
				(t (set-square (set-square s keeperPos blank) (list keeperRowPos (- keeperColPos 1)) keeper))
		))
		((equal direction 'RIGHT)
			(cond
				; Case 1. Move from keeperstart to blank
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list keeperRowPos (+ keeperColPos 1)) keeper))
				; Case 2. Move from keepr to blank.
				(t (set-square (set-square s keeperPos blank) (list keeperRowPos (+ keeperColPos 1)) keeper))
		))
	)
)


; 6: handleFromKeeToStar (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; (2) direction is a direction to move from the keeper position. (symbol).
; (3) keeperPos is a position of keeper in a state s. (r c) (list)
; (4) keeperRowPos is a row position of keeper. r (number)
; (5) keeperColPos is a column position of keeper. c (number)
; (6) keeperOrKeeStar is a content either keeper or keeperstar. (number)
; 2. Return value: Return a updated state s'. (list)
; 3. Explaning the method handleFromKeeToStar: The program is able to handle the case that 
; keeper moves to the star through the method handleFromKeeToStar.


(defun handleFromKeeToStar (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar)


	(cond
		((equal direction 'UP)
			(cond
				; Case 1. Move from keeperstart to a star.
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list (- keeperRowPos 1) keeperColPos) keeperstar))
				; Case 2. Move from keepr to a star.
				(t (set-square (set-square s keeperPos blank) (list (- keeperRowPos 1) keeperColPos) keeperstar))
		))
		((equal direction 'DOWN)
			(cond
				; Case 1. Move from keeperstart to a star.
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list (+ keeperRowPos 1) keeperColPos) keeperstar))
				; Case 2. Move from keepr to a star.
				(t (set-square (set-square s keeperPos blank) (list (+ keeperRowPos 1) keeperColPos) keeperstar))
		))
		((equal direction 'LEFT)
			(cond
				; Case 1. Move from keeperstart to a star.
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list keeperRowPos (- keeperColPos 1)) keeperstar))
				; Case 2. Move from keepr to a star.
				(t (set-square (set-square s keeperPos blank) (list keeperRowPos (- keeperColPos 1)) keeperstar))
		))
		((equal direction 'RIGHT)
			(cond
				; Case 1. Move from keeperstart to a star.
				((isKeeperStar keeperOrKeeStar) (set-square (set-square s keeperPos star) (list keeperRowPos (+ keeperColPos 1)) keeperstar))
				; Case 2. Move from keepr to a star.
				(t (set-square (set-square s keeperPos blank) (list keeperRowPos (+ keeperColPos 1)) keeperstar))
		))
	)
)


; 7: handleFromKeeToBox (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; (2) direction is a direction to move from the keeper position. (symbol).
; (3) keeperPos is a position of keeper in a state s. (r c) (list)
; (4) keeperRowPos is a row position of keeper. r (number)
; (5) keeperColPos is a column position of keeper. c (number)
; (6) keeperOrKeeStar is a content either keeper or keeperstar. (number)
; 2. Return value: Return a updated state s'. (list)
; 3. Explaning the method handleFromKeeToBox: The program is able to handle the case that 
; keeper moves to the position having box through the method handleFromKeeToBox.


(defun handleFromKeeToBox (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar)


	(cond
		((equal direction 'UP)
			(cond
				; Case 1. KeeperStar box star => star keeper boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (- keeperRowPos 1) keeperColPos) keeper) (list (- keeperRowPos 2) keeperColPos) boxstar))
				; Case 2. Keeper box star => blank keeper boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (- keeperRowPos 1) keeperColPos) keeper) (list (- keeperRowPos 2) keeperColPos) boxstar))
				; Case 3. KeeperStar box blank => star keeper box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (- keeperRowPos 1) keeperColPos) keeper) (list (- keeperRowPos 2) keeperColPos) box))
				; Case 4. Keeper box blank => blank keeper box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (- keeperRowPos 1) keeperColPos) keeper) (list (- keeperRowPos 2) keeperColPos) box))
				; Case 5. KeeperStar box wall => NIL
				; Case 6. Keeper box wall => NIL
				; Case 7. KeeperStar box box => NIL
				; Case 8. Keeper box box => NIL
				; Case 9. KeeperStar box boxStar => NIL
				; Case 10. Keeper box boxStar => NIL
				(t NIL)
		))
		((equal direction 'DOWN)
			(cond
				; Case 1. KeeperStar box star => star keeper boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (+ keeperRowPos 1) keeperColPos) keeper) (list (+ keeperRowPos 2) keeperColPos) boxstar))
				; Case 2. Keeper box star => blank keeper boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (+ keeperRowPos 1) keeperColPos) keeper) (list (+ keeperRowPos 2) keeperColPos) boxstar))
				; Case 3. KeeperStar box blank => star keeper box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (+ keeperRowPos 1) keeperColPos) keeper) (list (+ keeperRowPos 2) keeperColPos) box))
				; Case 4. Keeper box blank => blank keeper box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (+ keeperRowPos 1) keeperColPos) keeper) (list (+ keeperRowPos 2) keeperColPos) box))
				; Case 5. KeeperStar box wall => NIL
				; Case 6. Keeper box wall => NIL
				; Case 7. KeeperStar box box => NIL
				; Case 8. Keeper box box => NIL
				; Case 9. KeeperStar box boxStar => NIL
				; Case 10. Keeper box boxStar => NIL
				(t NIL)
		))
		((equal direction 'LEFT)
			(cond
				; Case 1. KeeperStar box star => star keeper boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (- keeperColPos 1)) keeper) (list keeperRowPos (- keeperColPos 2)) boxstar))
				; Case 2. Keeper box star => blank keeper boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (- keeperColPos 1)) keeper) (list keeperRowPos (- keeperColPos 2)) boxstar))
				; Case 3. KeeperStar box blank => star keeper box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (- keeperColPos 1)) keeper) (list keeperRowPos (- keeperColPos 2)) box))
				; Case 4. Keeper box blank => blank keeper box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (- keeperColPos 1)) keeper) (list keeperRowPos (- keeperColPos 2)) box))
				; Case 5. KeeperStar box wall => NIL
				; Case 6. Keeper box wall => NIL
				; Case 7. KeeperStar box box => NIL
				; Case 8. Keeper box box => NIL
				; Case 9. KeeperStar box boxStar => NIL
				; Case 10. Keeper box boxStar => NIL
				(t NIL)
		))
		((equal direction 'RIGHT)
			(cond
				; Case 1. KeeperStar box star => star keeper boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (+ keeperColPos 1)) keeper) (list keeperRowPos (+ keeperColPos 2)) boxstar))
				; Case 2. Keeper box star => blank keeper boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (+ keeperColPos 1)) keeper) (list keeperRowPos (+ keeperColPos 2)) boxstar))
				; Case 3. KeeperStar box blank => star keeper box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (+ keeperColPos 1)) keeper) (list keeperRowPos (+ keeperColPos 2)) box))
				; Case 4. Keeper box blank => blank keeper box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (+ keeperColPos 1)) keeper) (list keeperRowPos (+ keeperColPos 2)) box))
				; Case 5. KeeperStar box wall => NIL
				; Case 6. Keeper box wall => NIL
				; Case 7. KeeperStar box box => NIL
				; Case 8. Keeper box box => NIL
				; Case 9. KeeperStar box boxStar => NIL
				; Case 10. Keeper box boxStar => NIL
				(t NIL)
		))
	)
)

; 8: handleFromKeeToBoxStar (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; (2) direction is a direction to move from the keeper position. (symbol).
; (3) keeperPos is a position of keeper in a state s. (r c) (list)
; (4) keeperRowPos is a row position of keeper. r (number)
; (5) keeperColPos is a column position of keeper. c (number)
; (6) keeperOrKeeStar is a content either keeper or keeperstar. (number)
; 2. Return value: Return a updated state s'. (list)
; 3. Explaning the method handleFromKeeToBoxStar: The program is able to handle the case that 
; keeper moves to the position having box on the star through the method handleFromKeeToBoxStar.

(defun handleFromKeeToBoxStar (s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar)


	(cond
		((equal direction 'UP)
			(cond
				; Case 1. KeeperStar boxStar star => star keeperStar boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (- keeperRowPos 1) keeperColPos) keeperstar) (list (- keeperRowPos 2) keeperColPos) boxstar))
				; Case 2. Keeper boxStar star => blank keeperStar boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (- keeperRowPos 1) keeperColPos) keeperstar) (list (- keeperRowPos 2) keeperColPos) boxstar))
				; Case 3. KeeperStar boxStar blank => star keeperStar box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (- keeperRowPos 1) keeperColPos) keeperstar) (list (- keeperRowPos 2) keeperColPos) box))
				; Case 4. Keeper boxStar blank => blank keeperStar box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list (- keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (- keeperRowPos 1) keeperColPos) keeperstar) (list (- keeperRowPos 2) keeperColPos) box))
				; Case 5. KeeperStar boxStar wall => NIL
				; Case 6. Keeper boxStar wall => NIL
				; Case 7. KeeperStar boxStar box => NIL
				; Case 8. Keeper boxStar box => NIL
				; Case 9. KeeperStar boxStar boxStar => NIL
				; Case 10. Keeper boxStar boxStar => NIL
				(t NIL)
		))
		((equal direction 'DOWN)
			(cond
				; Case 1. KeeperStar boxStar star => star keeperStar boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (+ keeperRowPos 1) keeperColPos) keeperstar) (list (+ keeperRowPos 2) keeperColPos) boxstar))
				; Case 2. Keeper boxStar star => blank keeperStar boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (+ keeperRowPos 1) keeperColPos) keeperstar) (list (+ keeperRowPos 2) keeperColPos) boxstar))
				; Case 3. KeeperStar boxStar blank => star keeperStar box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos star) (list (+ keeperRowPos 1) keeperColPos) keeperstar) (list (+ keeperRowPos 2) keeperColPos) box))
				; Case 4. Keeper boxStar blank => blank keeperStar box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list (+ keeperRowPos 2) keeperColPos)))) (set-square (set-square (set-square s keeperPos blank) (list (+ keeperRowPos 1) keeperColPos) keeperstar) (list (+ keeperRowPos 2) keeperColPos) box))
				; Case 5. KeeperStar boxStar wall => NIL
				; Case 6. Keeper boxStar wall => NIL
				; Case 7. KeeperStar boxStar box => NIL
				; Case 8. Keeper boxStar box => NIL
				; Case 9. KeeperStar boxStar boxStar => NIL
				; Case 10. Keeper boxStar boxStar => NIL
				(t NIL)
		))
		((equal direction 'LEFT)
			(cond
				; Case 1. KeeperStar boxStar star => star keeperStar boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (- keeperColPos 1)) keeperstar) (list keeperRowPos (- keeperColPos 2)) boxstar))
				; Case 2. Keeper boxStar star => blank keeperStar boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (- keeperColPos 1)) keeperstar) (list keeperRowPos (- keeperColPos 2)) boxstar))
				; Case 3. KeeperStar boxStar blank => star keeperStar box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (- keeperColPos 1)) keeperstar) (list keeperRowPos (- keeperColPos 2)) box))
				; Case 4. Keeper boxStar blank => blank keeperStar box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (- keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (- keeperColPos 1)) keeperstar) (list keeperRowPos (- keeperColPos 2)) box))
				; Case 5. KeeperStar boxStar wall => NIL
				; Case 6. Keeper boxStar wall => NIL
				; Case 7. KeeperStar boxStar box => NIL
				; Case 8. Keeper boxStar box => NIL
				; Case 9. KeeperStar boxStar boxStar => NIL
				; Case 10. Keeper boxStar boxStar => NIL
				(t NIL)
		))
		((equal direction 'RIGHT)
			(cond
				; Case 1. KeeperStar boxStar star => star keeperStar boxStar
				((AND (isKeeperStar keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (+ keeperColPos 1)) keeperstar) (list keeperRowPos (+ keeperColPos 2)) boxstar))
				; Case 2. Keeper boxStar star => blank keeperStar boxStar
				((AND (isKeeper keeperOrKeeStar) (isStar (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (+ keeperColPos 1)) keeperstar) (list keeperRowPos (+ keeperColPos 2)) boxstar))
				; Case 3. KeeperStar boxStar blank => star keeperStar box
				((AND (isKeeperStar keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos star) (list keeperRowPos (+ keeperColPos 1)) keeperstar) (list keeperRowPos (+ keeperColPos 2)) box))
				; Case 4. Keeper boxStar blank => blank keeperStar box
				((AND (isKeeper keeperOrKeeStar) (isBlank (get-square s (list keeperRowPos (+ keeperColPos 2))))) (set-square (set-square (set-square s keeperPos blank) (list keeperRowPos (+ keeperColPos 1)) keeperstar) (list keeperRowPos (+ keeperColPos 2)) box))
				; Case 5. KeeperStar boxStar wall => NIL
				; Case 6. Keeper boxStar wall => NIL
				; Case 7. KeeperStar boxStar box => NIL
				; Case 8. Keeper boxStar box => NIL
				; Case 9. KeeperStar boxStar boxStar => NIL
				; Case 10. Keeper boxStar boxStar => NIL
				(t NIL)
		))
	)
)


; 9: try-move (s direction).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; (2) direction is a direction to move from the keeper position. (symbol).
; 2. Return value: This function returns the state that is the result of moving the keeper in
; state S in direction D. (list)
; 3. Explaning the method handleFromKeeToBox: The program is able to handle the state that moves
; to the direction D. 

(defun try-move (s direction)

	(let* ((keeperPosCR (getKeeperPosition s 0))
			(keeperPos (list (second keeperPosCR) (first keeperPosCR)))
			(keeperRowPos (first keeperPos))
			(keeperColPos (second keeperPos))
			(keeperOrKeeStar (get-square s keeperPos)))
		(cond 
			((equal direction 'UP) 
				(cond
					; Case 1: The above position of keeper is a wall.
					((isWall (get-square s (list (- keeperRowPos 1) keeperColPos))) NIL)
					; Case 2: The above position of keeper is a blank.
					((isBlank (get-square s (list (- keeperRowPos 1) keeperColPos))) (handleFromKeeToBlank s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 3: The above position of keeper is a star.
					((isStar (get-square s (list (- keeperRowPos 1) keeperColPos))) (handleFromKeeToStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 4: The above position of keeper is box.
					((isBox (get-square s (list (- keeperRowPos 1) keeperColPos))) (handleFromKeeToBox s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 5: The above position of keeper is boxStar.
					((isBoxStar (get-square s (list (- keeperRowPos 1) keeperColPos))) (handleFromKeeToBoxStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))		
			))
			((equal direction 'DOWN)
				(cond
					; Case 1: The above position of keeper is a wall.
					((isWall (get-square s (list (+ keeperRowPos 1) keeperColPos))) NIL)
					; Case 2: The above position of keeper is a blank.
					((isBlank (get-square s (list (+ keeperRowPos 1) keeperColPos))) (handleFromKeeToBlank s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 3: The above position of keeper is a star.
					((isStar (get-square s (list (+ keeperRowPos 1) keeperColPos))) (handleFromKeeToStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 4: The above position of keeper is box.
					((isBox (get-square s (list (+ keeperRowPos 1) keeperColPos))) (handleFromKeeToBox s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 5: The above position of keeper is boxStar.
					((isBoxStar (get-square s (list (+ keeperRowPos 1) keeperColPos))) (handleFromKeeToBoxStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))		
			))
			((equal direction 'LEFT)
				(cond
					; Case 1: The above position of keeper is a wall.
					((isWall (get-square s (list keeperRowPos (- keeperColPos 1)))) NIL)
					; Case 2: The above position of keeper is a blank.
					((isBlank (get-square s (list keeperRowPos (- keeperColPos 1)))) (handleFromKeeToBlank s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 3: The above position of keeper is a star.
					((isStar (get-square s (list keeperRowPos (- keeperColPos 1)))) (handleFromKeeToStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 4: The above position of keeper is box.
					((isBox (get-square s (list keeperRowPos (- keeperColPos 1)))) (handleFromKeeToBox s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 5: The above position of keeper is boxStar.
					((isBoxStar (get-square s (list keeperRowPos (- keeperColPos 1)))) (handleFromKeeToBoxStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))		
			))
			((equal direction 'RIGHT)
				(cond
					; Case 1: The above position of keeper is a wall.
					((isWall (get-square s (list keeperRowPos (+ keeperColPos 1)))) NIL)
					; Case 2: The above position of keeper is a blank.
					((isBlank (get-square s (list keeperRowPos (+ keeperColPos 1)))) (handleFromKeeToBlank s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 3: The above position of keeper is a star.
					((isStar (get-square s (list keeperRowPos (+ keeperColPos 1)))) (handleFromKeeToStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 4: The above position of keeper is box.
					((isBox (get-square s (list keeperRowPos (+ keeperColPos 1)))) (handleFromKeeToBox s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))
					; Case 5: The above position of keeper is boxStar.
					((isBoxStar (get-square s (list keeperRowPos (+ keeperColPos 1)))) (handleFromKeeToBoxStar s direction keeperPos keeperRowPos keeperColPos keeperOrKeeStar))		
			))
		)
	)
)


; 10: next-states (s).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; 2. Return value: This function calls try-move in each of four directions from the current 
; position of the keeper and collect all returned values into a list, and then returns it. (list)
; 3. Explaning the method next-states: The program is able to return the list of all states that
; can be reached from the given state in one move.

(defun next-states (s)
	(let*
		((result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT))))
		(cleanUpList result))
)

(setq s1 '((1 1 1 1 1)
		(1 4 0 0 1)
		(1 0 2 0 1)
		(1 0 3 0 1)
		(1 0 0 0 1)
		(1 1 1 1 1)))

(setq s2 '((1 1 1 1 1)
		(1 0 0 4 1)
		(1 0 2 3 1)
		(1 0 0 0 1)
		(1 0 0 4 1)
		(1 1 1 1 1)))

(setq s3 '((1 1 1 1 1)
		(1 0 0 6 1)
		(1 0 2 0 1)
		(1 0 0 0 1)
		(1 4 0 4 1)
		(1 1 1 1 1)))

(setq s4 '((1 1 1 1 1)
		(1 0 2 4 1)
		(1 0 0 0 1)
		(1 0 0 0 1)
		(1 0 5 3 1)
		(1 1 1 1 1)))




; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; 11: h0 (s).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; 2. Return value: This function returns the constant 0. (number)
; 3. Explaning the method h0: The program is a trivial admissible beacuse it never overestimates 
; the cost of reaching to the goal sate.

(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.


; 12: h1 (s).
; 1. Arguments: 
; (1) s is a state of the game (list). 
; 2. Return value: This function returns the number of postions where content is box. (number)
; 3. Explaning the method h1: The program counts and adds the number of boxes row by row.
; 4. Is this heuristic admissible?
; => Yes, it is defintely huristic admissible because h1 does not overestimate the number of steps
; needed to reach to the goal state from the state s. Since keeper can only move at most one box ; per action, the minimum number of steps needed to reach to the goal state is at least the number
; of boxes. As a result, h1 does not overestimates the cost of actions.

(defun h1 (s)
	(cond
		((NULL s) 0)
		(t (+ (count box (car s)) (h1 (cdr s))))
	)
)


; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.



; 13: findL1Distan (pos1 pos2). <= Helper function of h805330751.
; 1. Arguments: 
; (1) pos1 is the first postion (r, c) (list).
; (1) pos2 is the second postion (r, c) (list).
; 2. Return value: This function returns the manhattan distance between pos1 and pos2. (number)
; 3. Explaning the method findL1Distan: The program is able to get the manhattan distance between 
; pos1 and pos2 through the method findL1Distan (pos1 pos2).


(defun findL1Distan (pos1 pos2)

	(let ((pos1Row (first pos1))
		(pos1Col (second pos1))
		(pos2Row (first pos2))
		(pos2Col (second pos2)))
		(+ (abs (- pos2Row pos1Row)) (abs (- pos2Col pos1Col)))
	)
)


; 14: findL2Distan (pos1 pos2). <= Helper function of h805330751.
; 1. Arguments: 
; (1) pos1 is the first postion (r, c) (list).
; (1) pos2 is the second postion (r, c) (list).
; 2. Return value: This function returns the euclidean distance between pos1 and pos2. (number)
; 3. Explaning the method findL2Distan: The program is able to get the euclidean distance
; distance between pos1 and pos2 through the method findL2Distan (pos1 pos2).


(defun findL2Distan (pos1 pos2)

	(let ((pos1Row (first pos1))
		(pos1Col (second pos1))
		(pos2Row (first pos2))
		(pos2Col (second pos2)))
		(ceiling (sqrt (+ (* (- pos2Row pos1Row) (- pos2Row pos1Row)) (* (- pos2Col pos1Col) (- pos2Col pos1Col)))))
	)
)

; 15: findContentPosRow (content rowList row col). <= Helper function of findContentPos.
; 1. Arguments: 
; (1) content is the content to find locations in a row(=rowList). (number).
; (2) rowList is a list to serach the content. (list).
; (3) row represents the nth row.
; (4) column represents the nth column.
; 2. Return value: This function returns the list of position having the content ((r,c)...) (list)
; 3. Explaning the method findContentPosRow: The program is able to get the positions of
; the content in a rowList.

(defun findContentPosRow (content rowList row col)

	(cond
		; Case 1: Finishing searching the rowList.
		((NULL rowList) NIL)
		(t
			(cond
				; Case 2: Finding the position having the content. (r,c) 
				; to the list.
				((equal content (first rowList)) (cons (list row col) (findContentPosRow content (cdr rowList) row (+ col 1))))
				; Case 3: Moving next to the column position.
				(t (findContentPosRow content (cdr rowList) row (+ col 1)))
			)
		)
	)
)


; 16: findContentPos (s content row). <= Helper function of h805330751.
; 1. Arguments: 
; (1) s is a state of the game (list). 
; (2) content is the content to find locations in a row(=rowList). (number).
; (3) row represents the nth row.
; 2. Return value: This function returns the list of position having the content ((r,c)...) (list)
; 3. Explaning the method findContentPosRow: The program is able to get the positions of
; the content in a row.

(defun findContentPos (s content row)

	(cond
		; Case 1: Finishing searching the state s.
		((NULL s) NIL)
		(t
			; Case 2: Finding the position having the content and appending the postion (r,c) 
			; to the list.
			(let ((firstPos (findContentPosRow content (car s) row 0)))
				(append firstPos (findContentPos (cdr s) content (+ row 1)))
			)
		)
	)
)


; 17: chkBoxCorner (s boxesPos). <= Helper function of h805330751.
; 1. Arguments:
; (1) s is a state of the game (list). 
; (2) boxesPos is positions of boxes in state s. ((r, c)...) (list)
; 2. Return value: This function returns the true if the program finds the position of box that
; is located at the corner. In contrast, the function returns NIL if the program fail to
; find the postion of box that is located at the corner.
; 3. Explaning the method chkBoxCorner: The program is able to check whether there exists a box
; that is located at the corner.

(defun chkBoxCorner (s boxesPos)

	(cond
		; Case 1: Box that is located at the corner does not exist.
		((NULL boxesPos) NIL)
		(t
			(let* ((boxPos (first boxesPos))
				(upPos (get-square s (list (- (first boxPos) 1) (second boxPos))))
				(downPos (get-square s (list (+ (first boxPos) 1) (second boxPos))))
				(leftPos (get-square s (list (first boxPos) (- (second boxPos) 1))))
				(rightPos (get-square s (list (first boxPos) (+ (second boxPos) 1)))))
				(cond
					; Case 2: Upper-left corner
					((AND (isWall upPos) (isWall leftPos)) t)
					; Case 3: Bottom-left corner
					((AND (isWall downPos) (isWall leftPos)) t)
					; Case 4: Upper-right corner
					((AND (isWall upPos) (isWall rightPos)) t)
					; Case 5: Bottom-right corner
					((AND (isWall downPos) (isWall rightPos)) t)
					(t (chkBoxCorner s (cdr boxesPos)))											
				)
			)
		)
	)
)

; 18: findMinCostAboxToStars (boxPos starsPos) <= Helper function of h805330751.
; 1. Arguments:
; (1) boxesPos is positions of boxes in state s. ((r, c)...) (list)
; (2) starsPos is positions of stars in state s. ((r, c)...) (list)
; 2. Return value: This function returns the minimum cost between a box to stars. (number)
; 3. Explaning the method findMinCostAboxToStars: The program is able to get the minimum cost
; between a box to stars by using the function findMinCostAboxToStars.


(defun findMinCostAboxToStars (boxPos starsPos)

	(cond
		; Case 1: Left only one star to check the distance from a box.
		((equal (length starsPos) 1) (findL2Distan boxPos (car starsPos)))
		(t
			; Case 2: Get the minimum euclidean distance between a box and stars.
			(let ((firstDist (findL2Distan boxPos (first starsPos))))
			(min firstDist (findMinCostAboxToStars boxPos (cdr starsPos)))))
	)
)


; 19: getMinCostBoxesToStars (boxesPos starsPos) <= Helper function of h805330751.
; 1. Arguments:
; (1) boxesPos is positions of boxes in state s. ((r, c)...) (list)
; (2) starsPos is positions of stars in state s. ((r, c)...) (list)
; 2. Return value: This function returns the sum of minimum cost of a box to stars. (number)
; 3. Explaning the method findMinCostAboxToStars: The program is able to get the sum of minimum cost
; of a box to stars by using the function getMinCostBoxesToStars.

(defun getMinCostBoxesToStars (boxesPos starsPos)
	
	(cond
		; Case 1: Left only one box to check the minimum distance between a box to stars.
		((NULL boxesPos) 0)
		; Case 2: Get the sum of minimum euclidean distance between a box and stars.
		(t (+ (findMinCostAboxToStars (car boxesPos) starsPos) (getMinCostBoxesToStars (cdr boxesPos) starsPos)))
	)
)


; 20: findMinFromKeeToBS (keeperPos contentsPos) <= Helper function of h805330751.
; 1. Arguments:
; (3) keeperPos is a position of keeper in a state s. (r c) (list)
; (2) contentsPos is positions of contents in state s. ((r, c)...) (list)
; 2. Return value: This function returns the minimum cost between keeper to contents. (number)
; 3. Explaning the method findMinFromKeeToBS:  The program is able to get the minimum cost
; between keeper to contents by using the function findMinFromKeeToBS.

(defun findMinFromKeeToBS (keeperPos contentsPos)

	(cond
		; Case 1: Content does not exist.
		((NULL contentsPos) 0)
		; Case 2: Left only one content position to check the minimum distance between keeper to the content.
		((equal (length contentsPos) 1) (findL1Distan keeperPos (first contentsPos)))
		(t
			; Case 3: Get the minimum manhattan distance between keeper and contents.
			(let ((firstDist (findL1Distan keeperPos (first contentsPos))))
				(min firstDist (findMinFromKeeToBS keeperPos (cdr contentsPos))))
		)
	)
)


; 21: h805330751 (s)
; 1. Arguments:
; (1) s is a state of the game (list).
; 2. Return value: This function returns the sum of the minimum distance between keeper and boxes
; and the sum of minimum distance between a box and stars. (number)
; 3. Explaning the method h805330751:  The program is able to get the minimum cost between keeper and boxes
; and the sum of minimum cost between a box and stars by using the above helper functions.

(defun h805330751 (s)

	(let* ((keeperPosCR (getKeeperPosition s 0))
		; keeperPos is a position of keeper in a state s (r c) (list).
		(keeperPos (list (second keeperPosCR) (first keeperPosCR)))
		; boxesPos is positions of boxes in state s. ((r, c)...) (list)
		(boxesPos (findContentPos s box 0))
		; starsPos is positions of stars in state s. ((r, c)...) (list)
		(starsPos (findContentPos s star 0)))
		(cond
			; Check whether there exists a box that is located at the corner.
			; If there exists a box that is located at the corner,
			; we can just return the high value of cost 1500.
			((chkBoxCorner s boxesPos) 1500)
			(t
				; Set the sum of minimum euclidean distance between a box and stars as minSumBoxesToStars.
				(let ((minSumBoxesToStars (getMinCostBoxesToStars boxesPos starsPos)))
					(cond
						; Case 1: Returns the sum of the minimum distance between keeper and boxes 
						; and the sum of minimum distance between a box and stars if there exists a box 
						; that is not located on star.
						((< 0 minSumBoxesToStars) (+ (findMinFromKeeToBS keeperPos boxesPos) minSumBoxesToStars))
						; Case 2: Returns 0 if keeper is located  on the star and there is no box
						; that is not located on star
						((isKeeperStar (get-square s keeperPos)) 0)
						; Case 3: Returns the minimum manhattan distance between keeper and star
						; if keeper is not located on star and there is no box that is not located on star.
						(t (findMinFromKeeToBS keeperPos starsPos))
				))
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6) (o)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))


;(sokoban p1 #'h805330751)


;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))


;(sokoban p2 #'h805330751)


;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))


;(sokoban p3 #'h805330751)


;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))


;(sokoban p4 #'h805330751)


;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))


; (sokoban p5 #'h805330751)


;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))


;(sokoban p6 #'h805330751)


;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(sokoban p7 #'h805330751)


;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))


;(sokoban p8 #'h805330751)


;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(sokoban p9 #'h805330751)


;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))


;(sokoban p10 #'h805330751)


;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))


;(sokoban p11 #'h805330751)


;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))


;(sokoban p12 #'h805330751)


;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))


;(sokoban p13 #'h805330751)



;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))


;(sokoban p14 #'h805330751)


;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))



;(sokoban p15 #'h805330751)


; (print (findContentPos p5 box 0))
; (print (findContentPos p10 box 0))
; (print (findContentPos p13 box 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
