ski-lisp
========

An experimental, bottom-up SKI-based Lisp in Clojure.

Goals
- Build intuition for combinator evaluation as tree rewrites (S duplicates, K discards).
- Define a small, pure SKI core plus Church encodings for booleans, numerals, and lists.
- Provide bridges to Clojure (e.g., church->int, church->seq) and a tiny visual stepper.

Layout
- `src/ski_lisp/ski.clj` — SKI combinators (S, K, I, B, W), Church booleans/numerals/lists, arithmetic, and bridges.
- `src/ski_lisp/rewrite.clj` — SKI term AST, normal-order reducer, and pretty-printer to visualize rewrites.
- `test/ski_lisp` — small `clojure.test` suites and a simple test runner alias.

Usage
- Demo (traces + bridges): `clj -M:demo`.
- REPL: `clj` (or `clojure`) then `(require '[ski-lisp.ski :as s])`.
- Tests: `clj -M:test`.

Examples (REPL)
```clojure
(require '[ski-lisp.ski :as s])
(s/church->int s/two)                       ;=> 2
(s/church->int (s/suc s/two))               ;=> 3
(let [three (s/suc s/two)]
  [(s/church->int ((s/plus s/two) three))   ;=> 5
   (s/church->int ((s/mul s/two) three))    ;=> 6
   (s/church->int ((s/pow s/two) three))])  ;=> 8
(s/church->seq ((s/CONS :a) ((s/CONS :b) s/NIL))) ;=> [:a :b]
```

Stepper (visual intuition)
- Quick start: `clj -M:demo` prints step-by-step traces for K, S, B (via S/K), and the successor shape `((((S B) n) f) x) -> f (n f x)`.
- Or explore manually:
  ```clojure
  (require '[ski-lisp.rewrite :as r])
  (def B* (r/ap (r/ap (r/S) (r/ap (r/K) (r/S))) (r/K)))
  (def succ-app (r/ap* (r/S) B* (r/var :n) (r/var :f) (r/var :x)))
  (r/trace succ-app 8)
  ```

License: MIT
