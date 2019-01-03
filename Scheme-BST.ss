;; 7 discussed with Joanna Garrett

(define empty-BST
	(lambda ()
		'()))
		
(define empty-BST?
	(lambda (obj)
		(if (null? obj) #t #f)))
		
(define BST-insert
	(lambda (num bst)
		; if bst is empty, add num
		(if (null? bst)
			(list num '() '())
			; if num already in bst and it is our current elem, return bst
			(if (= num (car bst))
				bst
				; if num is smaller than the current elem, look at the cadr
				(if (< num (car bst))
					(list (car bst) (BST-insert num (cadr bst)) (caddr bst))
				; otherwise, look at the caddr, because num is greater than cur elem
					(list (car bst) (cadr bst) (BST-insert num (caddr bst))))))))
					
(define BST-inorder
	(lambda (bst)
		; if bst is null, return empty list
		(if (null? bst)
			'()
			(append (BST-inorder (cadr bst)) (list (car bst)) (BST-inorder (caddr bst))))))
			
(define BST?
	(lambda (obj)
				; must be lists
		(cond	[(not (list? obj)) #f]
				; can be empty
				[(null? obj) #t]
				; must have length three with first elem an integer
				[(not (and (= (length obj) 3) (integer? (car obj)))) #f]
				; children must be lists
				[(or (not (list? (cadr obj))) (not (list? (caddr obj)))) #f]
				; inorder rendering must be increasing (catches both bad order and repeats)
				[(not (increasing-inorder? obj)) #f]
				; must decrease to left and decrease to right or have null children
				[(and	(or (null? (cadr obj)) (< (car (cadr obj)) (car obj)))
						(or (null? (caddr obj)) (> (car (caddr obj)) (car obj))))
							(and (BST? (cadr obj)) (BST? (caddr obj)))]
				[else #f])))
				
(define increasing-inorder?
	(lambda (tree)
		; if the tree's inorder rendering doesn't change when we sort it
		(increasing-list? (BST-inorder tree))))
		
(define increasing-list?
	(lambda (ls)
		; true if the list is empty or one long
		(if (or (null? ls) (= 1 (length ls)))
			#t
			; false if the second elem is less than or equal to the first
			(if (<= (cadr ls) (car ls))
				#f
				; otherwise check rest of list
				(increasing-list? (cdr ls))))))
							
(define BST-element
	(lambda (bst)
		(car bst)))
		
(define BST-left
	(lambda (bst)
		(cadr bst)))
		
(define BST-right
	(lambda (bst)
		(caddr bst)))
		
(define BST-insert-nodes
	(lambda (bst nums)
		(BST-insert-nodes-help bst nums '())))
		
(define BST-insert-nodes-help
	(lambda (bst nums retBst)
		; if nums is empty
		(if (null? nums)
			retBst
			; if there are still nums, put them in the first in and recur
			(BST-insert-nodes-help bst (cdr nums) (BST-insert (car nums) retBst)))))
			
(define BST-contains?
	(lambda (bst num)
				; if the bst is empty
		(cond	[(null? bst) #f]
				; if the current elem is num
				[(= num (car bst)) #t]
				; if the current elem is too small, check right
				[(< (car bst) num) (BST-contains? (BST-right bst) num)]
				; if the current elem is too big, check left
				[(> (car bst) num) (BST-contains? (BST-left bst) num)]
				[else #f])))
				
(define BST-height
	(lambda (bst)
		; if you've reached a null node, return -1
		(if (null? bst)
			-1
			; otherwise, add one to the height of your tallest child
			(+ 1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst)))))))