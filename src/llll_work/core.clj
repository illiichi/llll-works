(ns llll-work.core
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all]
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
  (sin-osc :-freq))

(l4/control :test :-freq {:set 880})
(l4/finish)
(defsynth-l4 drum [freq 1000]
  (* 128
     (bpf (white-noise) freq 0.1)
     (env-gen (env-perc 0.05 dur) :action FREE)))

(defpattern hoge
  {:table {:a (control :test :-freq)
           :d (synth drum)}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32}
  (&|
   (->> (| :d * (inc (mod count 8)))
        (=| {:cycle? (= (mod count 2) 0)} :freq (if (= (mod count 4) 0)
                       [1000 800]
                       (range 400 1000 100))))
   (--> (| :a*8) 220 > (* (inc (mod count 3)) 1500)
        [(inc (mod count 4))] 880 >| 300)))



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

(defpattern test-control
  {:table {:a (control :test :-freq)}
   :period 64}
  (-> (| :a * 8)
       (--> 2 > [2] 20 [2] >| 1)))


(defsound test
  {:period 32
   :state {:initial {:count 1}
           :update (fn [m] (update m :count inc))}}

  (splay (* (map #(let [freq (* % (u/sin-r (u/sin-r 0.03 0.2 2.3) 0.99 1.01)
                                (!! (case (mod count 4)
                                      0 55, 1 62, 2 70, 3 40)))]
                    (* (/ 1 %) (sin-osc freq) (sin-osc (u/switch (!! (= (mod count 3) 0))
                                                                 (u/dq :-gate [0 15 0 0 8 0 10])
                                                                 (!! (* (mod count 9) 10))))))
                 (shuffle (range 1 12)))
            (env-gen (env-perc 0.05 (* dur (!! (* (inc (mod count 6)))))) (impulse (/ 1 dur))))))

(defsound test
  {:swap-option {:switch-dur 8}}
  (let [freq ;; (u/dq (u/pattern :-gate [1 0 0 0 0 1 1 1 0]) [200 220 280 110 110])
        :-freq
        ]
    (-> (splay (repeatedly 5 #(* (u/sin-r 1.22 0.8 1)
                                 (rlpf (pulse (u/sin-r 0.23 freq (* 1.01 freq)) (u/sin-r 0.3 0.1 0.9))
                                       (u/sin-r 3.8 1000 1100) 0.8))))
        (free-verb (u/sin-r 0.1 0.5 1) (u/sin-r 0.1 0.1 1)))))

(defpattern test-control
  {:table {:a (control :test :-freq)}
   :state {:initial {:count 0}
           :update (fn [m] (update m :count inc))}
   :period 32}
  (-> (| :a*16)
      (--> 100 [(mod count 4)] (* 120 (inc (mod count 3))) - (* 80 (inc (mod count 3)))
           > [3] 100 >| 200)))



