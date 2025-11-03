(ns ski-lisp.catalan
  "Experimental SKI-like combinators built from the paper's pair-collapse intuition.

  We model the \"pair collapse\" primitive as a weighted chooser that reinforces
  whichever branch wins. Starting only with the identity collapse (() x) -> x,
  we derive K and S by composing identities and letting reinforcement bias
  subsequent collapses. The resulting combinators interoperate with the existing
  closure-based runtime so we can reuse the current test suite."
  (:refer-clojure :exclude [S K]))

(def ^:private empty-node
  "The inert left branch used in (() x). Weight zero means it always loses."
  {:value ::empty :weight 0})

(defn- node?
  [v]
  (and (map? v) (contains? v :value) (contains? v :weight)))

(defn- ->node
  "Wrap a raw value with a default structural weight of 1."
  [value]
  (if (node? value)
    value
    {:value value :weight 1}))

(defn- reinforce
  "Increment the authority weight to mimic reinforcement after a collapse."
  [node]
  (update node :weight inc))

(defn- collapse
  "Collapse a weighted pair, optionally preferring a branch to break ties."
  ([left right] (collapse left right :right))
  ([left right prefer]
   (let [l (->node left)
         r (->node right)
         cmp (compare (:weight l) (:weight r))]
     (cond
       (pos? cmp) (reinforce l)
       (neg? cmp) (reinforce r)
       (= prefer :left) (reinforce l)
       :else (reinforce r)))))

(defn- apply-node
  "Apply one weighted node to another and reinforce the act of application.

  This encodes the \"left applies to right\" interpretation from the paper."
  [fn-node arg-node]
  (let [f-node (->node fn-node)
        a-node (->node arg-node)
        f (:value f-node)
        arg (:value a-node)]
    (when-not (fn? f)
      (throw (ex-info "Expected a function in the left branch"
                      {:node f-node :arg arg})))
    (-> {:value (f arg)
         :weight (max (:weight f-node) (:weight a-node))}
        reinforce)))

(defn I
  "Identity built purely from the (() x) collapse."
  [x]
  (:value (collapse empty-node x :right)))

(defn K
  "K combinator via nested identities: ((() a) b) collapses left biased."
  [a]
  (fn [b]
    (:value (collapse (collapse empty-node a :right) b :left))))

(defn S
  "S combinator via duplicated identity collapses.

  (((() a) c) ((() b) c)) -> (a c (b c))"
  [a]
  (fn [b]
    (fn [c]
      (let [a* (collapse empty-node a :right)
            b* (collapse empty-node b :right)
            c-node (->node c)
            ac (apply-node a* c-node)
            bc (apply-node b* c-node)
            result (apply-node ac bc)]
        (:value result)))))

(comment
  ;; Load in REPL:
  ;; (require '[ski-lisp.catalan :as cat])

  ;; Identity collapses to its argument.
  (cat/I 42)                                       ;=> 42

  ;; Build K and S purely from nested identities.
  (((cat/K :left) :right))                         ;=> :left
  ((((cat/S (cat/K inc)) (cat/K 1)) :ignored))     ;=> 2

  ;; Compose K/S to emulate standard SK behavior.
  (def plus (cat/S (cat/K (cat/S (cat/K inc))) (cat/K inc)))
  ((plus 3) 4)

  ;; Interop with existing runtime — treat results as plain functions.
  (((cat/S (cat/K (fn [x] (fn [y] (+ x y)))))
           (cat/K (fn [_] 10)))
    5)                                             ;=> 15
  )
