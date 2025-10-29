(ns ski-lisp.bracket
  "Bracket abstraction: compile untyped lambda calculus (λ) to SKI.

  This module provides:
  - A tiny λ-term AST (var/lam/app)
  - A compiler `compile-lam` that eliminates lambdas via bracket abstraction
    using the canonical rules:
      [x] x            → I
      x ∉ FV(M)        → [x] M = (K M)
      [x] (M N)        → (S ([x] M) ([x] N))
    Multi-arg lambdas are handled by repeated abstraction: [x y] M = [x]([y] M).
  - Optional eta-reduction pass (off by default):
      [x] (M x) where x ∉ FV(M) → M
    implemented as the SKI pattern: S (K M) I → M
  - An evaluator `eval-ski` for the SKI AST that uses the runtime
    combinators from `ski-lisp.ski` and an env map for free variables.

  The SKI AST produced here is compatible with `ski-lisp.rewrite` so you can
  pipe it into the stepper for structural traces.
  "
  (:require [ski-lisp.ski :as s]
            [clojure.set :as set]))

;; =============================
;; Lambda AST (for compilation)
;; =============================

(defn lvar
  "Construct a λ variable node: (lvar :x)."
  [name]
  [:var name])

(defn llam
  "Construct a λ abstraction: (llam :x body)."
  [x body]
  [:lam x body])

(defn lapp
  "Construct a λ application: (lapp f a)."
  [f a]
  [:app f a])

(defn free-vars-lam
  "Compute the set of free variables in a λ-term. Returns a set of names."
  [t]
  (let [[op a b] t]
    (case op
      :var #{a}
      :lam (disj (free-vars-lam b) a)
      :app (set/union (free-vars-lam a) (free-vars-lam b))
      #{})))

;; =============================
;; SKI AST (compatible with stepper)
;; =============================

(defn svar [] [:S])
(defn kvar [] [:K])
(defn ivar [] [:I])
(defn s-var [name] [:var name])
(defn s-ap [f g] [:ap f g])
(defn s-ap* [& terms] (reduce s-ap terms))

(defn free-vars-ski
  "Free variables in a SKI term (used for the K rule)."
  [t]
  (let [[op a b] t]
    (case op
      :var #{a}
      :ap (set/union (free-vars-ski a) (free-vars-ski b))
      #{})))

;; ==================================
;; Compilation (λ → SKI) and evaluation
;; ==================================

(declare BA eta-reduce)

(defn compile-lam
  "Compile a λ-term (using lvar/llam/lapp) into an equivalent SKI term.

  Strategy: Recursively compile subterms to SKI (`C`). For λ, apply a local
  bracket abstraction (`BA'`) directly on the compiled body to eliminate the
  bound variable. This avoids any naming collisions with the public `BA`.
  Optionally run `eta-reduce` when `:eta? true`.
  "
  ([t] (compile-lam t {:eta? false}))
  ([t {:keys [eta?]}]
   (letfn [(BA' [x e]
             (let [[op a b] e]
               (cond
                 (and (= op :var) (= a x)) (ivar)
                 (not (contains? (free-vars-ski e) x)) (s-ap (kvar) e)
                 (= op :ap) (s-ap (s-ap (svar) (BA' x a)) (BA' x b))
                 :else (throw (ex-info "Unsupported SKI form for BA'" {:x x :term e})))))
           (C [t]
             (let [[op a b] t]
               (case op
                 :var (s-var a)
                 :app (s-ap (C a) (C b))
                 :lam (BA' a (C b))
                 (throw (ex-info "Unknown λ node" {:node t})))))]
     (let [ski (C t)]
       (if eta? (eta-reduce ski) ski)))))

(defn eval-ski
  "Interpret a SKI AST to a Clojure value using runtime combinators.

  - `env` maps free variable names (keywords) to Clojure values/functions.
  - S/K/I nodes use implementations from `ski-lisp.ski` (curried closures).
  - Application `[:ap f g]` becomes `(eval-ski f env) applied to (eval-ski g env)`.
  "
  ([t] (eval-ski t {}))
  ([t env]
   (let [[op a b] t]
     (case op
       :S s/S
       :K s/K
       :I s/I
       :var (if (contains? env a)
              (get env a)
              (throw (ex-info "Unbound variable in SKI eval" {:name a})))
       :ap (let [f (eval-ski a env)
                 x (eval-ski b env)]
             (f x))
       (throw (ex-info "Unknown SKI node" {:node t}))))))

(defn apply*
  "Apply a curried function to a sequence of args."
  [f & args]
  (reduce (fn [acc x] (acc x)) f args))

;; ==================================
;; Bracket abstraction (on SKI terms)
;; ==================================

(defn BA
  "Apply bracket abstraction to eliminate variable `x` from SKI term `t`.

  Implements the standard rules:
  - If t is the variable x       → I
  - If x not free in t           → (K t)
  - If t = (m n)                 → (S ([x] m) ([x] n))
  "
  [x t]
  (let [[op a b] t]
    (cond
      ;; [x] x → I
      (and (= op :var) (= a x)) (ivar)

      ;; If x not free in t → (K t)
      (not (contains? (free-vars-ski t) x)) (s-ap (kvar) t)

      ;; Application: [x] (m n) → (S ([x] m) ([x] n))
      (= op :ap) (s-ap (s-ap (svar) (BA x a)) (BA x b))

      :else (throw (ex-info "Unsupported SKI form for bracket" {:x x :term t}))))

;; ==================================
;; Eta reduction (optional optimization)
;; ==================================

(defn eta-reduce
  "Reduce the SKI eta pattern: (S (K M) I) → M. Applies recursively.
  Idempotent: safe to run multiple times."
  [t]
  (letfn [(ap? [x] (and (vector? x) (= :ap (first x))))
          (step [term]
            (let [[op a b] term]
              (if (= op :ap)
                (let [l (step a)
                      r (step b)]
                  (if (and (ap? l)
                           (= [:S] (second l))
                           (ap? (nth l 2))
                           (= [:K] (second (nth l 2)))
                           (= [:I] r))
                    ;; Extract M from (K M)
                    (nth (nth l 2) 2)
                    [:ap l r]))
                term)))]
    (step t)))

;; =============================
;; Reader-commented examples
;; =============================

#_(do
    ;; Identity: λx.x  →  I
    (def id (compile-lam (llam :x (lvar :x))))
    id                                        ;=> [:I]

    ;; Constant: λx.y  →  K y
    (def konst (compile-lam (llam :x (lvar :y))))
    konst                                     ;=> [:ap [:K] [:var :y]]

    ;; Composition: λx. f (g x)   →   S (K f) (S (K g) I)
    (def comp' (compile-lam (llam :x (lapp (lvar :f) (lapp (lvar :g) (lvar :x))))))
    comp'

    ;; Successor (Church): λn f x. f (n f x)  →  ~ S B (after normalization)
    (def suc' (compile-lam (llam :n (llam :f (llam :x (lapp (lvar :f)
                                                          (lapp (lapp (lvar :n) (lvar :f)) (lvar :x))))))))
    suc')
  )
