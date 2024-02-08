
;;;; examples of dialogue

> (ql:quickload :puzzle-1617)
(:PUZZLE-1617)
> (in-package :puzzle-1617)
#<PACKAGE "PUZZLE-1617">

> ;;;; read puzzle problem from file
> (load-problem-file-into-puzzle "puzzle_10.txt")
;; puzzle problem here


> ;;;; write to HTML readed puzzle points
> (write-point-list-list-as-html "solved-sample.html"
                                 (load-problem-file-into-puzzle "puzzle_8.txt"))
HTML file updated at : /home/maki/programs/procon_solve/test/results/solved-sample.html 
#P"/home/maki/programs/procon_solve/test/results/solved-sample.html"


> ;;;; write to HTML solution
> (write-solven-puzzle-as-html "solved-sample.html"
                               (load-problem-file-into-puzzle "puzzle_10.txt"))
HTML file updated at : /home/maki/programs/procon_solve/test/results/solved-sample.html 
#P"/home/maki/programs/procon_solve/test/results/solved-sample.html"



