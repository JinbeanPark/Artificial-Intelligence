; Jinbean Park - 805330751
; Programming Assignment 2

; Problem 1.
; 1. Arguments: (1) TREE is a list that the program will do breadth-first search from left to right.
; 2. Return value: The program will return a single, top-level list of the terminal nodes in the order
; the program visited by a left to right BFS. (Return list)
; 3. Explaining my solutions: If the program encounters list type of element while searching a tree,
; the program append the list to the end of the tree after removing one layer of parentheses.
; In contrast, if the program encounters atom type of element while searching a tree, the program
; puts the atom type of element into the output list by using cons. If the tree becomes empty after
; processing every element in the list, it will return NIL.

(defun BFS (tree)

    (cond
        ; Case 1. Base case is when the tree is NULL after processing every element in a tree.
        ((NULL tree) NIL)
        ; Case 2. If the program encounters atom type element, it is appended to the visited output list.
        ((atom (car tree)) (cons (car tree) (BFS (cdr tree))))
        ; Case 3. If the program encounters list type element, the program append the list to the
        ; end of the tree after removing one layer of parentheses.
        (t (BFS (append (cdr tree) (car tree))))
    )
)




; Problem 2.
; 1. Arguments: (1) TREE is a list that the program will do depth-first search from right to left.
; 2. Return value: The program will return a single, top-level list of the terminal nodes in the order
; the program visited by a right to left DFS. (Return list)
; 3. Explaining my solutions: The idea of my solution is replacing the tree with the reversed order tree.
; To convert the order into the reversed order, the program appends the list of element 
; to the end of the output list if the element is atom while searching a list from left to right.
; Otherwise, the program appends the list to the end of the output list, passing the list to the 
; DFS function. As a result, the list that is passed to the DFS function will be appended to the 
; end of the ouput list after recursively converting the order of elements into the reverse order.

(defun DFS (tree)

    (cond
        ; Case 1: Base case is when tree is NULL.
        ((NULL tree) NIL)
        ; Case 2: If the first element is atom, it is appended to the end of output list after 
        ; being put into the list.
        ((atom (car tree)) (append (DFS (cdr tree)) (list (car tree))))
        ; Case 3: If the first element is list, it is appended to the end of output list, passing
        ; the list to the DFS function.
        (t (append (DFS (cdr tree)) (DFS (car tree))))
    )
)




; Problem 3.

; 1) Function 1 - removeParentheses (tree)
; (1) Arguments: (1) tree is a list that will be removed one layer of parentheses.
; (2) Return value: The program will return a list that is removed one layer of parenthesis (Return list)
; Strip off one layer of parantheses
; (3) Explanining the function removeParentheses (tree): The function removeParentheses removes
; one layer of parentheses of a list element in a tree, and then return the list that is removed
; one layer of parentheses.

(defun removeParentheses (tree)

    (cond
        ((NULL tree) NIL)
        ((atom (car tree)) (cons (car tree) (removeParentheses (cdr tree))))
        (t (append (car tree) (removeParentheses (cdr tree))))
    )
)


; 1) Function 2 - findDepthLimited (tree len)
; (1) Arguments: (1) tree is a list that the program will search to pick only atom type elements.
;                (2) len is a length of tree.
; (2) Return value: The program will pick only atom type elements in a tree, searching as much as 
; the value of len, and then the list consisted of picked elements will be returned.
; In other words, the program will search the elements until the limited level (Return list).
; (3) Explanining the function findDepthLimited (tree len): The function findDepthLimited picks 
; only atoms in a tree, searching a tree as much as the value of len. Thus, the program searches
; a tree until the specific limited level through the function findDepthLimited.

(defun findDepthLimited (tree len)

    (cond
        ((equal len 0) NIL)
        ((atom (car tree)) (cons (car tree) (findDepthLimited (cdr tree) (- len 1))))
        ((listp (car tree)) (findDepthLimited (cdr tree) (- len 1)))
    )
)


; 1) Function 3 - DFID (tree depth)
; (1) Arguments: (1) tree is a list that the program will do DFID search from left to right.
;                (2) depth is an integer representing the maximum depth of the tree.
; (2) Return value: The program will return a single, top-level list of the terminal nodes 
; in the order the program visited by a left to right DFID. (Return list).
; (3) Explanining the function DFID (tree depth): The function DFID searches a tree from
; depth = 0 to depth = maximum depth, decreasing depth by 1 whenever it searches one level of 
; depth of the tree. If the depth becomes 0, it will return NIL.
; If the tree is NIL, it means that there is no node to search, so it returns NIL.
; If the tree is atom, it means that tree only has a root node, so the program should
; return a root node as many as the value of (depth + 1) as a single list type.

(defun DFID (tree depth)

    (cond
        ((NULL tree) NIL)
        ((AND (atom tree) (< -1 depth)) (cons tree (DFID tree (- depth 1))))
        ((AND (atom tree) (< depth 0)) NIL)
        ((equal depth 0) NIL)
        (t (append (findDepthLimited tree (length tree))
            (DFID (removeParentheses tree) (- depth 1))))
    )
)




; Problem 4.
; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond
    ; If the state s is a final state, it returns true.
    ((equal s '(3 3 NIL)) t)
    ; Otherwise, it returns NIL.
    (t NIL)
  )
)


; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).

(defun next-state (s m c)
  
  (cond
    ; Case 1. The number of m and c is 0 or bigger than 2.
    ((OR (equal 0 (+ m c)) (< 2 (+ m c))) NIL)
    ; Case 2. Either m or c is greater than the number of m or c on this side of the river
    ((OR (< (first s) m) (< (second s) c)) NIL)
    ; Case 3. The number of c is bigger than m in either the west side or the east side 
    ;         when the number of missionaries is greater than 0.
    ((OR (AND (< 0 (- (first s) m)) (< (- (first s) m) (- (second s) c))) 
      (AND (< 0 (+ (- 3 (first s)) m)) (< (+ (- 3 (first s)) m) (+ (- 3 (second s)) c)))) NIL)
    ; Case 4. Returns the state applied an operator to the current state s.
    (t 
      (list (list (+ (- 3 (first s)) m) (+ (- 3 (second s)) c) (NOT (third s)))))
  )
)


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
    ; Case 1. Move one m.
    (append (next-state s 1 0)
    ; Case 2. Move one c.
    (next-state s 0 1)
    ; Case 3. Move one m and one c.
    (next-state s 1 1)
    ; Case 4. Move two m.
    (next-state s 2 0)
    ; Case 5. Move two c.
    (next-state s 0 2))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond
    ; Case 1: Base case is when states is NIL.
    ((NULL states) NIL)
    ; Case 2: s is equal to one of memebers of states.
    ((equal s (car states)) t)
    ; Case 3: s is not equal to one of memebers of states.
    (t (on-path s (cdr states)))
  )
)


; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond
    ; When there is no successor states.
    ((NULL states) NIL)
    ; It will move to the terminal node.
    ; If it encounteres the final state, it stops moving to the terminal node and
    ; returns the path from the initial state to the goal state.
    ; If the path was already visited, the value of chkFinalPath will be NIL and
    ; the program moves to sibling nodes or the grand parent node and
    ; starts moving into the terminal node recursively.
    (t (let ((chkFinalPath (mc-dfs (first states) path)))
        (cond
          ; If the value of chkFinalPath is not NIL, it means that the value of
          ; chkFinalPath is the path from the initial state to the goal state,
          ; and then the path will be returned.
          ; If the value of chkFinal path is NIL, it means that the path was already
          ; visited or there is no successor states, so the program will move to sibling nodes
          ; or the grand parent node and starts moving into the terminal node recursively.
          (chkFinalPath chkFinalPath)
          (t (mult-dfs (cdr states) path))
        )
    ))
  )
)


; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  
  (cond
    ; If the state s is equal to the final-state (3, 3, NIL), it returns the path
    ; from the initial state to the goal state.
    ((final-state s) (cons s path))
    ; If the state s was already visited, it returns NIL.
    ((on-path s path) NIL)
    ; After getting every successor states for s, it passes every sucessor states
    ; with the path.
    (t (let ((succList (succ-fn s)))
      (mult-dfs succList (cons s path))
    ))
  )
)

; (print (mc-dfs '(3 3 T) NIL))

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (print (next-state '(3 3 t) 1 0)) ;-> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (print (next-state '(3 3 t) 0 1)) ;-> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (print (succ-fn '(3 3 t))) ;-> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (print (succ-fn '(1 1 t))) ;-> ((3 2 NIL) (3 3 NIL))