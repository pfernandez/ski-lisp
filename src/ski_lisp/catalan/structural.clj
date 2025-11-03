(ns ski-lisp.catalan.structural
  "Pure data representation of Catalan-derived combinators.

  Values are persistent maps of the form {:kind :comb :name ... :args [...]},
  so reduction is structural (tree rewrites), not host-function closures.
  Conversion back to runnable functions happens via `realize`, which simply
  interprets the data description."
  (:refer-clojure :exclude [apply]))

(defn- comb
  [name & args]
  {:kind :comb
   :name name
   :args (vec args)})

(defn comb?
  [v]
  (and (map? v) (= (:kind v) :comb)))

(declare apply-value)

(def identity-comb
  (comb :I))

(def K-comb
  (comb :K))

(def S-comb
  (comb :S))

(defn- apply-comb
  [{:keys [name args] :as node} arg]
  (case name
    :I arg
    :K (if (empty? args)
         (comb :K arg)
         (first args))
    :S (case (count args)
         0 (comb :S arg)
         1 (clojure.core/apply comb :S (conj args arg))
         2 (let [[a b] args
                 a* (apply-value identity-comb a)
                 b* (apply-value identity-comb b)
                 ac (apply-value a* arg)
                 bc (apply-value b* arg)]
             (apply-value ac bc)))
    (throw (ex-info "Unknown combinator" {:node node :arg arg}))))

(defn- apply-host
  [f arg]
  (if (fn? f)
    (f arg)
    (throw (ex-info "Cannot apply non-function value" {:value f :arg arg}))))

(defn apply-value
  "Apply structural combinator or host function to arg, returning either a new
  combinator (for partial application) or the resulting value."
  [f arg]
  (cond
    (comb? f) (apply-comb f arg)
    (fn? f) (apply-host f arg)
    :else (throw (ex-info "Value is not applicable" {:value f :arg arg}))))

(defn realize
  "Turn a combinator description into a runnable (curried) Clojure function."
  [comb-node]
  (letfn [(step [state]
            (fn [x]
              (let [result (apply-value state x)]
                (if (comb? result)
                  (step result)
                  result))))]
    (step comb-node)))

(def I identity-comb)
(def K K-comb)
(def S S-comb)

(comment
  ;; Structural data only
  I
  K
  S

  ;; Realize to host functions
  (def I* (realize I))
  (I* 42)                                       ;=> 42
  (((realize S) (realize K)) (realize K)))
