
;;;; examples of dialogue

> (ql:quickload :puzzle-1617)
(:PUZZLE-1617)
> (in-package :puzzle-1617)
#<PACKAGE "PUZZLE-1617">

> ;;;; read puzzle problem from file
> (load-problem-file-into-puzzle "puzzle_10.txt")
;; puzzle problem here

> ;;;; write to HTML piece list in overlap
> (write-piece-list-as-html "solved-sample.html"
                                       (load-problem-file-into-puzzle "puzzle_10.txt"))
HTML file updated at : /..../procon_solve/test/results/solved-sample.html 
#P"/..../procon_solve/test/results/solved-sample.html"

> ;;;; write to HTML solution
> (write-solven-puzzle-as-html "solved-sample.html"
                               (solve-puzzle (load-problem-file-into-puzzle "puzzle_10.txt")))
HTML file updated at : /..../procon_solve/test/results/solved-sample.html 
#P"/..../programs/procon_solve/test/results/solved-sample.html"



