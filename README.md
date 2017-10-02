
# Puzzle theory simulator
competition division of 28th programming contest of NCT

### bug (?)

```lisp

(defun is-nil-piece (piece)
  (>= 2 (length (piece-spots piece)))
      
  ;;(null (piece-spots piece)))
  )

(defun piece-shape= (piece1 piece2)
  ;; if it is scheme, i could write this more clearly...
  (let ((piece-list (synthesize-piece-list-all piece1 piece2)))
    (print (mapcar #'piece-spots piece-list))
    (find-if #'is-nil-piece
             piece-list)))
        

(defun test (piece)
  (piece-shape= piece piece))

(mapcar #'piece-shape= (cdr *test-piece-list1*) (cdr *test-piece-list1*))

;; doesn't return t all
;; vdr-queue adjust may have bug

```



### made by
Makinori Ikegami <maau3p@gmail.com>


# memos

## data-structure
```
Piece ::= {spots::[Spot], degrees::[Float], is-frame::Bool, synth-from::Synth||Nil, synth-to::Synth||Nil}
Synth ::= {Piece, direction::((+1||-1)::=::Bool), synth-from::Rotate-Ord}
Spot ::= x::Int, y::Int

in hashtable, 
element =:: table=::(Id::S), Piece
```

## coordinate-system

- point
- degree
- length ::= x^2+y^2 # square-length
- point--(inherit)-->coordinate


```
   y
   *
   |
   |
 --O--*x
   |
   |
          (0=2pi)
           (y=5)
        .....*.....
        .....|..A..
        .....|.....
        .....|.....
        .....|.....
(3/2*pi)-----O----* (x=5)(1/2*pi) 
        .....|.....
        .....|.....
        .....|.....
        ..B..|.....
        .....*.....
            (pi)
```

- point O
- point A
- point B
