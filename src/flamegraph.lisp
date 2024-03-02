
;; note
;; flamegraph for profiling
;; Ref: https://github.com/40ants/cl-flamegraph
;; Ref: https://github.com/brendangregg/FlameGraph

(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)

(ql:quickload :flamegraph)

(ql:quickload :puzzle-1617)


;; profile procedure and save flamegraph to *flame-file*
;; Open `https://www.speedscope.app/` in browser, Open local flamegraph file.

(defparameter *flame-file* "/tmp/profile-puzzle1617.stack")

(in-package :puzzle-1617)

(flamegraph:save-flame-graph (*flame-file*)
  (profile-easy-problem))
