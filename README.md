
# Puzzle theory simulator
competition division of 28th programming contest of NCT



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
