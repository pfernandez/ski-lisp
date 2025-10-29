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

Design: Closures vs Bracket Pipeline
------------------------------------
- Two layers
  - Closures runtime (`src/ski_lisp/ski.clj`): defines `S/K/I/B/W` and Church encodings as curried Clojure functions. Fast and simple — great for building functionality.
  - Bracket + reducer (`src/ski_lisp/bracket.clj`, `src/ski_lisp/rewrite.clj`): compile λ-terms to SKI AST (variable-free), and reduce via S/K/I rules with normal-order if desired.

- Why bracket abstraction?
  - Variable-free programs: λ → SKI removes variables and lambdas; evaluation becomes pure graph rewrite (no substitution or environments at runtime).
  - Control evaluation strategy: Clojure is strict (applicative order). SKI reduction in `rewrite.clj` can follow normal-order so `K` truly ignores its right arg, and you can step reductions explicitly.
  - Structural visibility and portability: SKI AST is plain data you can serialize, analyze, optimize (eta-reduction, recognize `B/C/W`), and eventually run in your own VM/WASM.

- When to use which
  - Use closures when you want quick, fast functions (Church numerals/booleans/lists) and don’t need to observe evaluation strategy.
  - Use bracket + reducer when you need normal-order semantics, visualization of structure (S duplication, K discard), or a serializable program representation.

- Convenience wrappers (in `ski-lisp.ski`)
  - `lambda->ski` compiles a λ AST (built with `ski-lisp.bracket/llam`, `lvar`, `lapp`) to SKI AST. Optional `{:eta? true}` shrinks via eta-reduction.
  - `lambda->fn` compiles and evaluates to a runnable Clojure function; provide an env for free vars if needed.
  ```clojure
  (require '[ski-lisp.ski :as s]
           '[ski-lisp.bracket :as b])
  ;; λx.x → I
  (s/lambda->ski (b/llam :x (b/lvar :x))) ;=> [:I]
  ;; λx. f (g x) with f=inc, g=inc
  (let [lam (b/llam :x (b/lapp (b/lvar :f) (b/lapp (b/lvar :g) (b/lvar :x))))
        f   (s/lambda->fn lam {:f inc :g inc})]
    (f 1)) ;=> 3
  ```

- Visualizing compiled terms
  ```clojure
  (require '[ski-lisp.rewrite :as r]
           '[ski-lisp.bracket :as b]
           '[ski-lisp.ski :as s])
  (def lam (b/llam :x (b/lapp (b/lvar :f) (b/lapp (b/lvar :g) (b/lvar :x)))))
  (def ski (s/lambda->ski lam))
  (r/trace (r/ap ski (r/var :x)) 6)
  ```

License: MIT
