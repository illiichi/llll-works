(ns llll-work.core
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all])
  (use [overtone.live])
  ;; (use [overtone.core])
  ;; eval below code to connect sc server
  ;; (connect-external-server "localhost" 57110) 
  )

(l4/initialize {})

(control :first-sound :vol {:set 1})
(control :first-sound :-freq {:set 800})

(defsound first-sound
  {:synth-option {:type :detune}}
  (sin-osc (* dr :-freq)))

(control :first-sound :-freq {:dur 32 :to 1600})
(do (control :first-sound :-freq {:dur 32 :to 100})
    (control :first-sound :-vol {:dur 32 :to 0}))

(l4/stop :first-sound)

(defsynth-l4 ping [freq 100]
  (* (pulse freq)
     (env-gen (env-perc 0.01 dur) :action FREE)))

(defsynth-l4 ping-2 [freq 100]
  (* (saw freq)
     (env-gen (envelope [0 1 1 0] [0.01 dur 0.03]) :action FREE)))

(defpattern first-pattern
  {:table {:p (synth ping)
           :p2 (synth ping-2)}
   :period 32}
  (&| (| :p2*8 :p|4)
      (->> (| :p*8|4 :p2*16|8)
           (=| :freq [300 400]))))

(l4/control :first-pattern :vol {:dur 16 :to 1})

(defsynth-l4 drum [freq 1000]
  (* 128
     (bpf (white-noise) freq 0.1)
     (env-gen (env-perc 0.05 dur) :action FREE)))


(defpattern first-pattern
  {:table {:a (control :first-sound :-freq)
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

(l4/stop :first-pattern)
(l4/finish)
