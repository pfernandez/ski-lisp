(ns ski-lisp.rewrite)

;; A tiny SKI term representation and normal-order reducer
;; to visualize structural rewrites (S duplicates, K discards).

;; ---------------------------------
;; Term constructors
;; ---------------------------------
(defn S [] [:S])
(defn K [] [:K])
(defn I [] [:I])
(defn var [name] [:var name])
(defn ap [f g] [:ap f g])
(defn ap* [& terms]
  (reduce (fn [a b] (ap a b)) terms))

;; B = S (K S) K as an AST (derived, not primitive)
(def B* (ap (ap (S) (ap (K) (S))) (K)))

;; ---------------------------------
;; Pretty printer
;; ---------------------------------
(defn pretty [t]
  (letfn [(pp [t]
            (let [[op a b] t]
              (case op
                :S "S"
                :K "K"
                :I "I"
                :var (name a)
                :ap (str "(" (pp a) " " (pp b) ")")
                (str t))))]
    (pp t)))

;; ---------------------------------
;; One normal-order reduction step
;; ---------------------------------
(defn ap? [t] (and (vector? t) (= :ap (first t))))
(defn op? [t tag] (and (vector? t) (= tag (first t))))

(defn reduce-once [t]
  ;; Returns {:term new-term :rule :S|:K|:I} or nil if no step
  (letfn [(step [term]
            (when (vector? term)
              (let [[op a b] term]
                (cond
                  (= op :ap)
                  (let [l a r b]
                    (cond
                      ;; K rule: ((K x) y) -> x
                      (and (ap? l) (op? (second l) :K))
                      {:term (nth l 2) :rule :K}

                      ;; S rule: (((S x) y) z) -> ((x z) (y z))
                      (and (ap? l)
                           (ap? (second l))
                           (op? (second (second l)) :S))
                      (let [lz (second l)
                            z r
                            y (nth l 2)
                            x (nth lz 2)]
                        {:term (ap (ap x z) (ap y z)) :rule :S})

                      ;; I rule: (I x) -> x
                      (op? l :I)
                      {:term r :rule :I}

                      :else
                      (or (when-let [res (step l)]
                            {:term (ap (:term res) r) :rule (:rule res)})
                          (when-let [res (step r)]
                            {:term (ap l (:term res)) :rule (:rule res)}))))

                  :else nil))))]
    (step t)))

;; ---------------------------------
;; Multi-step reduction
;; ---------------------------------
(defn reduce-steps
  ([t] (reduce-steps t 50))
  ([t limit]
   (loop [i 0 cur t acc []]
     (if (>= i limit)
       acc
       (if-let [{t' :term rule :rule} (reduce-once cur)]
         (recur (inc i) t' (conj acc {:term t' :rule rule}))
         acc)))))

(defn trace
  "Pretty-print each reduction step up to `limit`."
  ([t] (trace t 50))
  ([t limit]
   (println (str "0:  " (pretty t)))
   (doseq [[i {:keys [term rule]}] (map-indexed vector (reduce-steps t limit))]
     (println (format "%d:  %-2s  %s" (inc i) (name rule) (pretty term))))))

;; ---------------------------------
;; Normalize to a (bounded) normal form
;; ---------------------------------
(defn normalize
  "Reduce `t` by repeatedly applying `reduce-once` until no step is possible
  or `limit` is reached. Returns the resulting term."
  ([t] (normalize t 200))
  ([t limit]
   (loop [i 0 cur t]
     (if (>= i limit)
       cur
       (if-let [{t' :term} (reduce-once cur)]
         (recur (inc i) t')
         cur)))))

;; ---------------------------------
;; Reader-commented demo
;; ---------------------------------

#_(do
    ;; ((((S B) n) f) x) -> f (n f x)
    (def succ-app (ap* (S) B* (var :n) (var :f) (var :x)))
    (trace succ-app 12))
