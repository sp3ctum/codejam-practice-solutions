(ns codejam_practice_solutions.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   ))

(enable-console-print!)

#_
(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'codejam_practice_solutions.core-test))
    0
    1))
