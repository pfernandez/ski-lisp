(ns ski-lisp.ski-test
  (:require [clojure.test :refer :all]
            [ski-lisp.ski :as s]))

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
         (s/church->seq ((s/CONS :a) ((s/CONS :b) s/NIL))))))

