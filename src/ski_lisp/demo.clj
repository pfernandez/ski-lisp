(ns ski-lisp.demo
  (:require [ski-lisp.rewrite :as r]
            [ski-lisp.ski :as s]))

(defn heading [t]
  (println)
  (println "==" t))

(defn demo-rewrite []
  (heading "K discards right: ((K a) b) -> a")
  (let [expr (r/ap* (r/ap (r/K) (r/var :a)) (r/var :b))]
    (r/trace expr 2))

  (heading "S duplicates argument: (((S f) g) x) -> ((f x) (g x))")
  (let [expr (r/ap* (r/ap (r/ap (r/S) (r/var :f)) (r/var :g)) (r/var :x))]
    (r/trace expr 2))

  (heading "B via S and K: (((B f) g) x) -> (f (g x))")
  (let [expr (r/ap* r/B* (r/var :f) (r/var :g) (r/var :x))]
    (r/trace expr 8))

  (heading "Successor shape: ((((S B) n) f) x) -> f (n f x)")
  (let [expr (r/ap* (r/S) r/B* (r/var :n) (r/var :f) (r/var :x))]
    (r/trace expr 8)))

(defn demo-bridge []
  (heading "Church numerals and arithmetic")
  (println "zero one two three:" (map s/church->int [s/zero s/one s/two (s/suc s/two)]))
  (let [three (s/suc s/two)]
    (println "two+three:" (s/church->int ((s/plus s/two) three)))
    (println "two*three:" (s/church->int ((s/mul s/two) three)))
    (println "two^three:" (s/church->int ((s/pow s/two) three))))

  (heading "Church booleans")
  (println "is-zero zero?" (s/church->bool (s/is-zero s/zero)))
  (println "is-zero one? " (s/church->bool (s/is-zero s/one)))

  (heading "Church lists")
  (println "[:a :b] ->"
           (s/church->seq ((s/CONS :a) ((s/CONS :b) s/NIL)))))

(defn -main [& _]
  (demo-rewrite)
  (demo-bridge))

