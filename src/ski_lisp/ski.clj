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
