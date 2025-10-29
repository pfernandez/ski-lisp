(ns ski-lisp.rewrite-test
  (:require [clojure.test :refer :all]
            [ski-lisp.rewrite :as r]))

(defn nf [t]
  (loop [cur t]
    (if-let [{:keys [term]} (r/reduce-once cur)]
      (recur term)
      cur)))

(deftest constructors-and-pretty
  (is (= [:S] (r/S)))
  (is (= [:K] (r/K)))
  (is (= [:I] (r/I)))
  (is (= [:var :a] (r/var :a)))
  (is (= [:ap [:K] [:var :a]] (r/ap (r/K) (r/var :a))))
  (is (= [:ap [:ap [:K] [:var :a]] [:var :b]]
         (r/ap* (r/K) (r/var :a) (r/var :b))))
  (is (= "((K a) b)"
         (r/pretty (r/ap* (r/K) (r/var :a) (r/var :b))))))

(deftest reduction-rules
  ;; K rule
  (let [expr (r/ap* (r/K) (r/var :a) (r/var :b))]
    (is (= {:term (r/var :a) :rule :K}
           (r/reduce-once expr)))
    (is (= (r/var :a) (nf expr))))

  ;; I rule
  (let [expr (r/ap (r/I) (r/var :x))]
    (is (= {:term (r/var :x) :rule :I}
           (r/reduce-once expr)))
    (is (= (r/var :x) (nf expr))))

  ;; S rule (one step)
  (let [expr (r/ap* (r/ap (r/ap (r/S) (r/var :f)) (r/var :g)) (r/var :x))
        expected (r/ap (r/ap (r/var :f) (r/var :x)) (r/ap (r/var :g) (r/var :x)))]
    (is (= {:term expected :rule :S}
           (r/reduce-once expr))))

  ;; B* behaves like composition
  (let [expr (r/ap* r/B* (r/var :f) (r/var :g) (r/var :x))
        expected (r/ap (r/var :f) (r/ap (r/var :g) (r/var :x)))]
    (is (= expected (nf expr)))))

