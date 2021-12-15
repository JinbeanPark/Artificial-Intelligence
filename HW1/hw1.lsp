; Jinbean Park - 805330751
; Programming Assignment 1

; Problem 1.
; 1. Arguments: (1) N is a number that the program tries to check whether N exists in TREE or not.
;               (2) TREE is a list that the program will explore to find N.
; 2. Return value: The program will return t if the program finds N in TREE while the program will
;    return NIL if the program fail to find N in TREE. (Return boolean)
; 3. Explaining my solutions: The idea of my solution is checking the second element of TREE m 
; (L m R) recursively by passing L and R as argument when m is not equal to N.
; If m is equal to N, the program will return t. However, if m is not equal to N, L and R will be 
; passed as argument to the function TREE-CONTAINS recursively. While exploring L and R, 
; if N has been found in either L or R, the program returns t while the program returns NIL 
; if N has not been found in either L or R.

(defun TREE-CONTAINS (N TREE)
    (cond 
        ; Case 1: TREE is the number. Return t if N is equal to TREE. Otherwise, return NIL.
        ((numberp TREE) (equal N TREE))
        ; Case 2: The second element of TREE m (L m R) is equal to N.
        ((equal (second TREE) N) t)
        ; Case 3: Passes the L and R as argument to TREE-CONTAINS.
        ; Return t if either TREE-CONTAINS N (first TREE) or TREE-CONTAINS N (third TREE) returns t.
        ; Otherwise, return NIL.
        (t (OR (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (third TREE))))
    )
)


; (print "Problem 1 - Test cases")
; (print (TREE-CONTAINS 3 '((1 2 3) 7 8))) ; T
; (print (TREE-CONTAINS 4 '((1 2 3) 7 8))) ; NIL
; (print (TREE-CONTAINS 3 3)) ;T
; (print (TREE-CONTAINS 4 3)) ;NIL
; (print (TREE-CONTAINS 0 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) ;NIL
; (print (TREE-CONTAINS 3 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) ;T
; (print (TREE-CONTAINS 4 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) ;NIL
; (print (TREE-CONTAINS 5 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) ;T
; (print (TREE-CONTAINS 14 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) ;NIL



; Problem 2.
; 1. Arguments: (1) TREE is a list that the program will explore to find minimum value in TREE.
; 2. Return value: The program will return minimum value in TREE. (Return number)
; 3. Explaining my solutions: The idea of my solution is finding the leftmost number in TREE
; since the minimum number must be existing at leftmost in TREE by the property of ordered tree.
; Therefore, the program keep searching the TREE until encountering the number as the first element
; of TREE.

(defun TREE-MIN (TREE)
    (cond
        ; Case 1: TREE is a number, not a list. In this case, minimum value is TREE
        ; since TREE has only one number of value.
        ((numberp TREE) TREE)
        ; Case 2: The first element of TREE is list.
        ; Minimum value is leftmost number of TREE, so the program should search 
        ; the first element of list to find the leftmost number in TREE.
        ((listp (car TREE)) (TREE-MIN (car TREE)))
        ; Case 3: The first element of TREE is number.
        ; The first element of TREE is the number means that this number is the leftmost number in
        ; TREE. Therefore, this number is the minimum number.
        ((numberp (car TREE)) (car TREE)))
)


; (print "Problem 2 - Test cases")
; (print (TREE-MIN '((1 2 3) 7 8))) ;1
; (print (TREE-MIN 3)) ;3
; (print (TREE-MIN '((2 3 4) 5 (6 8 (9 10 (11 12 13)))))) ;2
; (print (TREE-MIN '(1 2 3))) ;1
; (print (TREE-MIN '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) ; (5 2 1 3 8 6 10 9 12 11 13) ;1



; Problem 3.
; 1. Arguments: (1) TREE is a list that the program will convert it into pre-ordered list.
; 2. Return value: The program will return pre-ordered TREE list. (Return list)
; 3. Explaining my solutions: The idea of my solution is visiting m (L m R) first by setting
; the m as first element of pre-ordered list, and then move to left child node by passing the
; first element TREE as argument, and then it has to move to right child node by passing the
; the third element TREE as argument. Therefore, the program will visit m and move to left 
; and move to right recursively until visiting every node in TREE.

(defun TREE-ORDER (TREE)
    (cond
        ; Case 1: TREE is the number means that TREE does not have any child node,
        ; so it is base case where the program returns (TREE).
        ((numberp TREE) (list TREE))
        ; Case 2: In order to convert ordered trees into pre-ordered list, the program has to
        ; visit m (L m R) first, and then it has to move to left child node by passing the 
        ; first element TREE as argument, and then it has to move to right child node by passing
        ; the third element TREE as argument.
        (t (cons (second TREE) (append (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE)))))
    )
)


; (print "Problem 3 - Test cases")
; (print (TREE-ORDER 3)) ;(3)
; (print (TREE-ORDER '((1 2 3) 7 8))) ;(7 2 1 3 8)
; (print (TREE-ORDER '(1 2 3))) ;(2 1 3)
; (print (TREE-ORDER '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))) ; (5 2 1 3 8 6 10 9 12 11 13)




; Problem 4.
; 1. Arguments: (1) L is a list that the program will explore to find and return the sub-list.
;               (2) START is the number and starting position of the sub-list.
;               (3) LEN is the number and length of the sub-list.
; 2. Return value: The program will return sub-list having the starting position as START and the
; length as LEN. (Return list)
; 3. Explaining my solutions: The idea of my solution is finding the starting position of sub-list
; by subtracting 1 from START and passing the rest of L. If START becomes zero, it starts appending
; the first element of L, and it keeps appending the first element of L while subtracting 1 from
; LEN. If LEN becomes zero, it means that the program has reached to the end position of sub-list.

(defun SUB-LIST (L START LEN)
    (cond
        ; Case 1: The length is equal to zero means that it has reached to 
        ; the end position of sub-list.
        ((= LEN 0) NIL)
        ; Case 2: The value of START is equal to zero means that it has reached to 
        ; the starting position of sub-list.
        ((= START 0) (cons (car L) (SUB-LIST (cdr L) 0 (- LEN 1))))
        ; Case 3: The program keep running on the case 3 until reaching to the starting position
        ; of sub-list by passing the subtracted START and tail of L to the function SUB-LIST recursively.
        (t (SUB-LIST (cdr L) (- START 1) LEN))
    )
)


; (print "Problem 4 - Test cases")
; (print (SUB-LIST '(a b c d) 0 3)) ;(A B C)
; (print (SUB-LIST '(a b c d) 3 1)) ; (D)
; (print (SUB-LIST '(a b c d) 2 0)) ; NIL
; (print (SUB-LIST '(a b c d) 4 1)) ; NIL



; Problem 5.
; 1. Arguments: (1) L is a list that the program will split L into two sub-lists L1 and L2.
; 2. Return value: The program will return two sub-lists L1 and L2, which length of L1 minus
; length of L2 is 0 or 1.
; 3. Explaining my solutions: The idea of my solution is checking the length L first. If the
; length of L is odd, the length of L1 should be (L + 1) / 2, but the length of L1 should be (L / 2) 
; if the length of L is even. After getting the length of L1, we can return (list L1 L2) by using
; the length of L1 and calling the function SUB-LIST.

(defun SPLIT-LIST (L)
    (let 
        ((lenL1 
            (cond
                ((oddp (length L)) (/ (+ (length L) 1) 2))
                (t (/ (length L) 2))
        )))
        (list (SUB-LIST L 0 lenL1) (SUB-LIST L lenL1 (- (length L) lenL1)))
    )
)


; (print "Problem 5 - Test cases")
; (print (SPLIT-LIST '(a))) ;((A) NIL) 
; (print (SPLIT-LIST '(a b))) ;((A) (B))
; (print (SPLIT-LIST '(a b c))) ;((A B) (C)) 
; (print (SPLIT-LIST '(a b c d))) ;((A B) (C D))
; (print (SPLIT-LIST '(a b c d e))) ;((A B C) (D E))
; (print (SPLIT-LIST '(a b c d e f))) ;((A B C) (D E F)) 



; Problem 6.
; 1. Arguments: (1) TREE is binary tree where the program will find the height.
; 2. Return value: The program will return the height of TREE where the length of the longest
; path from the root node to the farthest leaf node.
; 3. Explaining my solutions: The idea of my solution is choosing the longest path between
; L and R (L R). In order to get the longest length of path, the program recursively keep moving to 
; either the left child or the right until reaching to the atom. The program checks 
; and choose the paths which is greater between left and right, and then the program sets 
; the greater path as lenMax and increases the length of lenMax by 1. 
; In the end, the program returns the longest path from the root node to the farthest leaf node.

(defun BTREE-HEIGHT (TREE)
    (cond
        ; Case 1: TREE is a leaf node.
        ((atom TREE) 0)
        ; Case 2: TREE is a list (L R).
        (t (let* ((lenL (BTREE-HEIGHT (first TREE)))
                  (lenR (BTREE-HEIGHT (second TREE)))
                  ; Find the maximum length between lenL and lenR.
                  (lenMax (cond 
                            ((> lenL lenR) lenL)
                            (t lenR))))
           (+ 1 lenMax)                 
           )
        )
    )
)

; (print "Problem 6 - Test cases")
; (print (BTREE-HEIGHT 1)) ;0
; (print (BTREE-HEIGHT '(1 2))) ;1
; (print (BTREE-HEIGHT '(1 (2 3)))) ;2
; (print (BTREE-HEIGHT '((1 2) (3 4)))) ;2
; (print (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))) ;3
; (print (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))) ;3



; Problem 7.
; 1. Arguments: (1) LEAVES is a non-empty list of atoms.
; 2. Return value: The program will return the binary tree such that (1) The tree leaves are
; the elements of LEAVES (2) For any internal (non-leaf) node in the tree, the number of leaves
; in its left branch minums the number of leaves in its right branch is 0 or 1.
; 3. Explaining my solutions: The idea of my solution is recursively spliting LEAVES into 
; two lists L1 and L2, keeping that the length of L1 minus the length L2 is 0 or 1.
; LEAVES is splitted iteratively until the length of LEAVES becomes 1 and the first element of
; LEAVES is atom. The format of return value is ((splitted L1) (splitted L2)).

(defun LIST2BTREE (LEAVES)
    (cond
        ; Case 1: The length LEAVES is 1 and the first element of LEAVES is atom.
        ((AND (equal (length LEAVES) 1) (atom (car LEAVES))) (car LEAVES))
        ; Split Leaves into two lists L1 and L2, which length of L1 minums length L2 is 0 or 1.
        ; Define leftBranch as L1 and rightBranch as L2.
        ; Repeat it until the length of sub-list become 1.
        (t 
            (let ((leftBranch (first (SPLIT-LIST LEAVES)))
                  (rightBranch (second (SPLIT-LIST LEAVES))))
                (list (LIST2BTREE leftBranch) (LIST2BTREE rightBranch))
            )
        )
    )
)


; (print "Problem 7 - Test cases")
; (print (LIST2BTREE '(1))) ;1
; (print (LIST2BTREE '(1 2))) ;(1 2)
; (print (LIST2BTREE '(1 2 3))) ;((1 2) 3)
; (print (LIST2BTREE '(1 2 3 4))) ;((1 2) (3 4))
; (print (LIST2BTREE '(1 2 3 4 5))) ;(((1 2) 3) (4 5))
; (print (LIST2BTREE '(1 2 3 4 5 6))) ;(((1 2) 3) ((4 5) 6)) 
; (print (LIST2BTREE '(1 2 3 4 5 6 7))) ;(((1 2) (3 4)) ((5 6) 7)) 
; (print (LIST2BTREE '(1 2 3 4 5 6 7 8))) ;(((1 2) (3 4)) ((5 6) (7 8))) 



; Problem 8.
; 1. Arguments: (1) TREE is the binary tree such that (1) The tree leaves are
; the elements of LEAVES (2) For any internal (non-leaf) node in the tree, the number of leaves
; in its left branch minums the number of leaves in its right branch is 0 or 1.
; 2. Return value: The program will return a non-empty list of atoms.
; 3. Explaining my solutions: The idea of my solution is stripping parentheses of the first element
; of TREE and combining all small lists into a whole one list. In order to stip parentheses,
; the program recursively combines two lists L and R from the small size of L and R to the big size
; of L and R. Therefore, the program will finally return one whole list of atoms.

(defun BTREE2LIST (TREE)
    (cond
        ; Case 1: TREE is NULL
        ((NULL TREE) NIL)
        ; Case 2: TREE is a atom.
        ((atom TREE) (list TREE))
        ; Case 3: The first element of TREE is atom
        ((atom (car TREE)) (cons (car TREE) (BTREE2LIST (cdr TREE))))
        ; Case 4: The first element of TREE is list
        ((listp (car TREE)) (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE))))
    )
)


(print "Problem 8 - Test cases")
(print (BTREE2LIST 1)) ;(1)
; (print (BTREE2LIST '(1 2))) ;(1 2)
; (print (BTREE2LIST '((1 2) 3))) ;(1 2 3)
; (print (BTREE2LIST '((1 2) (3 4)))) ;(1 2 3 4)
; (print (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7)))) ;(1 2 3 4 5 6 7)
; (print (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))) ; (1 2 3 4 5 6 7 8)



; Problem 9.
; 1. Arguments: (1) E1 and E2 are LISP expressions that can be either an atom or a list.
; 2. Return value: The program will return t if E1 and E2 are same. Otherwise, it will return NIL.
; 3. Explaining my solutions: The idea of my solution is considering three big cases and deciding whether 
; E1 and E2 are same or not. The first case is that both E1 and E2 are numbers. In this case, the program
; check whether two numbers E1 and E2 are same or not. The second case is that E1 is a number while
; E2 is a list or viceversa. In this case, the program returns NIL. The last case is that both E1
; and E2 are lists. In this case, the program checks two cases. The first one is checking 
; whether the length of E1 is equal to the length of E2 or not. The second one is checking
; whether the atoms in list of E1 and E2 are same or not.

(defun IS-SAME (E1 E2)
    (cond
        ; Case 1: Both E1 and E2 are NULL.
        ((AND (NULL E1) (NULL E2)) t)
        ; Case 2: E1 is a number and E2 is a list OR viceversa.
        ((OR (AND (numberp E1) (listp E2)) 
             (AND (listp E1) (numberp E2))) NIL)
        ; Case 3: Both E1 and E2 are number.
        ((AND (numberp E1) (numberp E2)) (= E1 E2))
        ; Case 4: The length of E1 is different to the length of E2. 
        ((NOT (= (length E1) (length E2))) NIL)
        ; Case 5: The first element of E1 is a number but the first element of E2 is a list
        ; or viceversa.
        ((OR (AND (numberp (car E1)) (listp (car E2))) 
             (AND (listp (car E1)) (numberp (car E2)))) NIL)
        ; Case 6: Both of the first element of E1 and E2 is a number, but two values are different.
        ((AND (numberp (car E1)) (numberp (car E2)) (NOT (= (car E1) (car E2)))) NIL)
        ; Case 7: Both of the first element of E1 and E2 is a number & two values are same.
        ((AND (numberp (car E1)) (numberp (car E2)) (= (car E1) (car E2))) (IS-SAME (cdr E1) (cdr E2)))
        ; Case 8: Both of the first element of E1 and E2 are lists.
        ((AND (listp (car E1)) (listp (car E2))) (AND (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))           
    )
)


; (print "Problem 9 - Test cases")
; (print (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8))) ;T
; (print (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8))) ;NIL
; (print (IS-SAME 2 2 )) ;T
; (print (IS-SAME 2 4 )) ;NIL
; (print (IS-SAME 2 '(1 3 2 7 8))) ;NIL
; (print (IS-SAME '(1 3 2 7 8) 2 )) ;NIL
; (print (IS-SAME '(1 2 3 7 8) '(1 2 3 7 8))) ;T
; (print (IS-SAME '(1 2 3 7 8) '(1 2 3 7))) ;NIL
; (print (IS-SAME '(1 2 3 7 8) '(1 2 4 7 8))) ;NIL
; (print (IS-SAME '(1 2 3 7 8) '(1 3 2 7 8))) ;NIL