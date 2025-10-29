(ns ski-lisp.bracket-test
  (:require [clojure.test :refer :all]
            [ski-lisp.bracket :as b]
            [ski-lisp.ski :as s]))

(deftest bracket-core-identities
  ;; λx.x  =>  I
  (is (= [:I] (b/compile-lam (b/llam :x (b/lvar :x)))))

  ;; λx.y  =>  (K y)
  (is (= [:ap [:K] [:var :y]]
         (b/compile-lam (b/llam :x (b/lvar :y))))))

(deftest bracket-composition-eval
  ;; λx. f (g x) applied with f=inc, g=inc, x=1  => 3
  (let [term (b/llam :x (b/lapp (b/lvar :f) (b/lapp (b/lvar :g) (b/lvar :x))))
        ski  (b/compile-lam term)
        f    (b/eval-ski ski {:f inc :g inc})]
    (is (= 3 (f 1)))))

(deftest bracket-multi-lambda-eval
  ;; λf.λg.λx. f (g x)  applied to inc inc 1  => 3
  (let [term (b/llam :f (b/llam :g (b/llam :x (b/lapp (b/lvar :f)
                                                       (b/lapp (b/lvar :g) (b/lvar :x))))))
        ski  (b/compile-lam term)
        comp (b/eval-ski ski)]
    (is (= 3 (b/apply* comp inc inc 1)))))

(deftest bracket-successor-church
  ;; λn f x. f (n f x) behaves like s/suc
  (let [term (b/llam :n (b/llam :f (b/llam :x (b/lapp (b/lvar :f)
                                                       (b/lapp (b/lapp (b/lvar :n) (b/lvar :f)) (b/lvar :x))))))
        ski  (b/compile-lam term)
        suc' (b/eval-ski ski)]
    (is (= 1 (s/church->int (b/apply* suc' s/zero) )))
    (is (= 1 (((b/apply* suc' s/zero) inc) 0)))
    (is (= 2 (((b/apply* suc' s/one) inc) 0))))
  ;; And check the closed-form equals s/suc by behavior on a couple inputs
  (let [suc-rt s/suc]
    (is (= 3 (((suc-rt s/two) inc) 0)))))

