(ns ski-lisp.ski)

;; =============================================
;; SKI core as explicit closures (no macros)
;; =============================================

(def S (fn [f] (fn [g] (fn [x] ((f x) (g x))))))
(def K (fn [x] (fn [_] x)))
(def I ((S K) K))

;; Derived helpers
(def B ((S (K S)) K))       ;; composition: B f g x = f (g x)
(def W ((S S) (K I)))       ;; duplication: W f x = f x x

;; =============================================
;; Church encodings and bridges
;; =============================================

;; Booleans
(def TRUE K)
(def FALSE (K I))
(def IF (fn [b] (fn [t] (fn [e] ((b t) e)))))
(def IFL
  (fn [b]
    (fn [t]
      (fn [e]
        ;; Lazy IF: t and e are 1-arg thunks; call only chosen one
        (((b t) e) nil)))))

;; Numerals
(def suc (S B))             ;; successor: suc n f x = f (n f x)
(def zero (K I))
(def one I)
(def two (suc one))

;; Arithmetic
(def plus (fn [m] (fn [n] ((n suc) m))))
(def mul  (fn [m] (fn [n] (fn [f] (m (n f))))))
(def pow  (fn [m] (fn [n] (n m))))
(def is-zero (fn [n] ((n (K FALSE)) TRUE)))

;; Pairs
(def PAIR (fn [x] (fn [y] (fn [f] ((f x) y)))))
(def FST (fn [p] (p TRUE)))
(def SND (fn [p] (p FALSE)))

;; Lists (Church-encoded)
(def NIL (K I))
(def CONS (fn [h] (fn [t] (fn [c] (fn [n] ((c h) ((t c) n)))))))

;; Bridges to Clojure
(defn church->int [n]
  ((n inc) 0))

(defn int->church [n]
  (fn [f]
    (fn [x]
      (loop [i n acc x]
        (if (zero? i)
          acc
          (recur (dec i) (f acc)))))))

(defn church-fold [n f seed]
  ((n f) seed))

(defn church->seq [lst]
  ;; Accumulate into a list (conj to front) to preserve order, then vec.
  (vec ((lst (fn [h] (fn [acc] (conj acc h)))) '())))

(defn seq->church [coll]
  (reduce (fn [t h] ((CONS h) t)) NIL (reverse coll)))

(defn church->bool [b]
  ((b true) false))

;; =============================================
;; Numeral predicates and comparison
;; =============================================

;; Helper for predecessor via pairs
(def PHI (fn [p]
           ((PAIR (SND p)) (suc (SND p)))))

(def pred
  (fn [n]
    (FST ((n PHI) ((PAIR zero) zero)))))

(def minus
  (fn [m]
    (fn [n]
      ((n pred) m))))

;; Boolean combinators
(def AND (fn [a] (fn [b] ((a b) FALSE))))
(def OR  (fn [a] (fn [b] ((a TRUE) b))))
(def NOT (fn [a] ((a FALSE) TRUE)))

(def eq
  (fn [m]
    (fn [n]
      ((AND (is-zero ((minus m) n))) (is-zero ((minus n) m))))))

(def leq
  (fn [m]
    (fn [n]
      (is-zero ((minus m) n)))))

(def lt
  (fn [m]
    (fn [n]
      (NOT ((leq n) m)))))

;; =============================================
;; Recursion (applicative-order fixpoint) and examples
;; =============================================

(def Z
  (fn [f]
    ((fn [x] (f (fn [v] ((x x) v))))
     (fn [x] (f (fn [v] ((x x) v)))))))

(def factorial
  (Z (fn [recurse]
       (fn [n]
         (((IFL (is-zero n))
           (fn [_] one))
          (fn [_] ((mul n) (recurse (pred n)))))))))

(def fibonacci
  (Z (fn [recurse]
       (fn [n]
         (((IFL (is-zero n))
           (fn [_] zero))
          (fn [_]
            (((IFL (is-zero (pred n)))
              (fn [_] one))
             (fn [_] ((plus (recurse (pred n))) (recurse (pred (pred n))))))))))))

;; =============================================
;; Lambda helpers (via bracket compiler)
;; =============================================

(defn lambda->ski
  "Compile a λ-term (built with ski-lisp.bracket lvar/llam/lapp) to SKI AST.
  Optional opts: {:eta? boolean} to enable eta-reduction.
  Uses requiring-resolve to avoid a compile-time circular dependency."
  ([lam]
   ((requiring-resolve 'ski-lisp.bracket/compile-lam) lam))
  ([lam opts]
   ((requiring-resolve 'ski-lisp.bracket/compile-lam) lam opts)))

(defn lambda->fn
  "Compile a λ-term to a runnable Clojure function using SKI runtime.
  Arities:
  - (lambda->fn lam)             ; closed term
  - (lambda->fn lam env)         ; open term, provide env {name -> value}
  - (lambda->fn lam opts env)    ; with {:eta? bool} and env
  Uses requiring-resolve to avoid a compile-time circular dependency."
  ([lam]
   (let [compile-lam (requiring-resolve 'ski-lisp.bracket/compile-lam)
         eval-ski    (requiring-resolve 'ski-lisp.bracket/eval-ski)]
     (eval-ski (compile-lam lam))))
  ([lam env]
   (let [compile-lam (requiring-resolve 'ski-lisp.bracket/compile-lam)
         eval-ski    (requiring-resolve 'ski-lisp.bracket/eval-ski)]
     (eval-ski (compile-lam lam) env)))
  ([lam opts env]
   (let [compile-lam (requiring-resolve 'ski-lisp.bracket/compile-lam)
         eval-ski    (requiring-resolve 'ski-lisp.bracket/eval-ski)]
     (eval-ski (compile-lam lam opts) env))))

;; =============================================
;; REPL examples (reader-commented)
;; =============================================

#_(church->int zero)                ;=> 0
#_(church->int one)                 ;=> 1
#_(church->int two)                 ;=> 2
#_(church->int (suc two))           ;=> 3

#_(let [three (suc two)]
    [(church->int ((plus two) three))  ;=> 5
     (church->int ((mul two) three))   ;=> 6
     (church->int ((pow two) three))]) ;=> 8

#_(church->bool (is-zero zero))     ;=> true
#_(church->bool (is-zero one))      ;=> false

#_(church->seq ((CONS :a) ((CONS :b) NIL))) ;=> [:a :b]

#_(do
    (require '[ski-lisp.bracket :as b])
    ;; λx.x compiled to SKI
    (lambda->ski (b/llam :x (b/lvar :x)))    ;=> [:I]
    ;; λx. f (g x) executed with env
    (let [lam (b/llam :x (b/lapp (b/lvar :f) (b/lapp (b/lvar :g) (b/lvar :x))))
          f   (lambda->fn lam {:f inc :g inc})]
      (f 1)))                                 ;=> 3
