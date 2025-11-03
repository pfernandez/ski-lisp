(ns ski-lisp.ski-test
  (:require [clojure.test :refer :all]
            [ski-lisp.ski :as s]
            [ski-lisp.catalan :as cat]
            [ski-lisp.bracket :as b]))

(deftest numerals-and-arith
  (is (= 0 (s/church->int s/zero)))
  (is (= 1 (s/church->int s/one)))
  (is (= 2 (s/church->int s/two)))
  (is (= 3 (s/church->int (s/suc s/two))))
  (let [three (s/suc s/two)]
    (is (= 5 (s/church->int ((s/plus s/two) three))))
    (is (= 6 (s/church->int ((s/mul s/two) three))))
    (is (= 8 (s/church->int ((s/pow s/two) three))))))

(deftest booleans-and-zero
  (is (= true  (s/church->bool (s/is-zero s/zero))))
  (is (= false (s/church->bool (s/is-zero s/one)))))

(deftest church-lists
  (is (= [:a :b]
         (s/church->seq ((s/CONS :a) ((s/CONS :b) s/NIL)))))
  (is (= [] (s/church->seq s/NIL)))
  (is (= [:x :y :z]
         (s/church->seq (s/seq->church [:x :y :z])))))

(def ^:private combinator-impls
  [{:label "closure runtime" :I s/I :K s/K :S s/S}
   {:label "pair collapse" :I cat/I :K cat/K :S cat/S}])

(deftest core-combinators
  (doseq [{:keys [label I K S]} combinator-impls]
    (testing (str label " I/K/S")
      ;; I: returns its argument
      (is (= 2 ((I inc) 1)))
      ;; K: returns first, ignoring second
      (is (= :x ((K :x) :y)))
      ;; S: duplicates x into f and g
      (is (= 2 (((S (K inc)) (K 1)) :ignored)))))
  (testing "derived closure helpers"
    ;; B: composition
    (is (= 3 (((s/B inc) inc) 1)))
    ;; W: duplicate argument to f
    (is (= [42 42] ((s/W (fn [a] (fn [b] [a b]))) 42)))))

(deftest pairs-and-booleans
  (let [p ((s/PAIR :a) :b)]
    (is (= :a (s/FST p)))
    (is (= :b (s/SND p))))
  (is (= :t (((s/IF s/TRUE) :t) :e)))
  (is (= :e (((s/IF s/FALSE) :t) :e)))
  (is (= true (s/church->bool s/TRUE)))
  (is (= false (s/church->bool s/FALSE))))

(deftest bridges-and-folds
  (is (= 5 (s/church->int (s/int->church 5))))
  (is (= 2 (s/church-fold s/two inc 0))))

(deftest pred-minus-compare
  (let [three (s/suc s/two)
        four  (s/suc three)]
    (is (= 0 (s/church->int (s/pred s/zero))))
    (is (= 1 (s/church->int (s/pred s/two))))
    (is (= 2 (s/church->int (s/pred three))))
    (is (= 1 (s/church->int ((s/minus s/two) s/one))))
    (is (= 0 (s/church->int ((s/minus s/two) three))))
    (is (= true  (s/church->bool ((s/eq s/two) s/two))))
    (is (= false (s/church->bool ((s/eq s/two) three))))
    (is (= true  (s/church->bool ((s/leq s/two) three))))
    (is (= false (s/church->bool ((s/leq four) three))))
    (is (= true  (s/church->bool ((s/lt s/two) three))))
    (is (= false (s/church->bool ((s/lt s/two) s/two))))))

(deftest recursion-factorial-fibonacci
  (is (= 1 (s/church->int (s/factorial s/zero))))
  (is (= 1 (s/church->int (s/factorial s/one))))
  (let [five (s/int->church 5)
        seven (s/int->church 7)]
    (is (= 120 (s/church->int (s/factorial five))))
    (is (= 13  (s/church->int (s/fibonacci seven))))))

(deftest lambda-wrappers
  ;; lambda->ski identity
  (is (= [:I] (s/lambda->ski (b/llam :x (b/lvar :x)))))
  ;; lambda->fn closed term
  (let [idf (s/lambda->fn (b/llam :x (b/lvar :x)))]
    (is (= 42 (idf 42))))
  ;; lambda->fn with env
  (let [lam (b/llam :x (b/lapp (b/lvar :f) (b/lapp (b/lvar :g) (b/lvar :x))))
        f   (s/lambda->fn lam {:f inc :g inc})]
    (is (= 3 (f 1)))))
