(ns llll-work.core
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(kill-server)

(l4/initialize {})
(l4/finish)

(defsound test
  {:period 32
   :synth-option {:type :detune}
   :swap-option {:fade-in-dur 8
                 :switch-fade-in 4
                 :switch-fade-out 8
                 :fade-out-dur 32}}
  (sin-osc 440))

(defsound test
  {:state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32
   :synth-option {:type :detune}
   :swap-option {:fade-in-dur 8
                 :switch-dur 4
                 :fade-out-dur 32}}
  (* (sin-osc (* :-freq (u/dq :-gate [300 600 400]) dr (u/rg-exp (lf-saw (/ 1 dur) -1)
                                                                 (!! (if (= (mod count 2) 0) 1 3))
                                                                 (!! (if (= (mod count 4) 0) 2 8)))))
     (env-gen (env-perc 0.05 :t8) (impulse :f4))))













