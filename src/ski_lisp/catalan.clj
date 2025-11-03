(ns ski-lisp.catalan
  "Catalan-derived SKI combinators realized from pure structural rewrites.

  The heavy lifting lives in `ski-lisp.catalan.structural`, which encodes I/K/S
  as persistent data and reduction rules. This namespace merely realizes those
  descriptions into runnable Clojure functions so the existing test suite and
  REPL demos keep working."
  (:refer-clojure :exclude [S K])
  (:require [ski-lisp.catalan.structural :as struct]))

(def I
  (struct/realize struct/I))

(def K
  (struct/realize struct/K))

(def S
  (struct/realize struct/S))

(comment
  ;; Structural descriptions
  struct/I
  struct/K
  struct/S

  ;; Runnable closures via structural interpreter
  (I 42)                                          ;=> 42
  ((K :left) :right)                              ;=> :left
  (((S (K inc)) (K 1)) :ignored)                  ;=> 2
  )
