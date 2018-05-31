(ns llll-work.thai-kick
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(kill-server)

(recording-start (str "/home/maeda/code/clojure/llll-work/record/"
                      (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (java.util.Date.))
                      ".wav"))
(recording-stop)

(l4/initialize {:osc-client false})
(l4/finish)


(defn update-state [{:keys [count] :as m}]
  (let [c (inc count)
        pairs (fn [prefix f]
                (map (fn [x] { (keyword (str prefix x)) (f x) })
                     (range 4)))]
    (-> m
        (assoc :count c)
     (into (pairs "b" #(bit-test c %)))
     (into (pairs "c" #(->> %
                            (bit-shift-right c)
                            (bit-and 2r0011)))))))

(do (l4/control :thai :-low {:dur 16 :to 4})
    (l4/control :thai :-high {:dur 16 :to 10}))

(do (l4/control :thai :-low {:dur 32 :to 0})
    (l4/control :thai :-high {:dur 32 :to 0}))

(defsound thai
  {:period 32
   :state {:initial (update-state {:count 0})
           :update update-state}
   :swap-option {:fade-in-dur 64}
   :synth-option {:type :detune}}
  (let [gate (lf-pulse :f4)
        seed-freq (lin-lin gate 0 1 800 950)
        freq (u/rg-lin (u/switch (!! b0) (u/m-map lf-pulse [1/10 0.34 0.25 2.5])
                                 (u/m-map #(lf-pulse % 0 1/8) [0.2 1/2 3 8]))
                       (* :-low seed-freq) (* :-high seed-freq))]
    (-> (* (sin-osc (reduce (fn [acc _] (* freq (sin-osc acc) dr))
                            seed-freq (range 3)))))))



(l4/control :thai :vol {:dur 128 :to 0})

(l4/control :kan :vol {:dur 16 :to 0})
(l4/control :jump :vol {:dur 16 :to 0})
(l4/control :star :vol {:dur 16 :to 0})

(defsound rythm
  {:period 32
   :synth-option {:type :detune}}
  (let [gate (lf-pulse :f4)
        seed-freq (lin-lin gate 0 1 800 900)
        freq (lin-lin (u/m-map lf-pulse [1/10 0.34 0.25 2.5])
                      0 1
                      (* 4 seed-freq) (* 10 seed-freq))]
    (+ (let [base-freq (u/dq gate [100 80])
             fenv (env-gen (envelope [0 8 1] [0.0001 0.01]) gate)]
         (* (sin-osc (* dr base-freq fenv) ) (env-gen (env-perc 0.001 0.1) gate)))
       (let [gate (impulse 8)]
         (* (lf-pulse 1) 100 (bpf (brown-noise) (* dr 1250) 0.01) gate))))  )

(l4/control :rythm :vol {:dur 128 :to 0})

(defsound light
  {:period 32}
  (splay (map (fn [freq phase]
                (let [gate (impulse (* 1/64 :f1) phase)]
                  (* 4 (pulse freq (lf-noise0 8))
                     (env-gen (env-perc :t8 :t16) gate))))
              (shuffle (u/n-range 5000 10000 8))
              (u/n-range 0 1 8))))

(demo 10 (let [gate (* (lf-pulse 1/2 0 7/8) (impulse 8))
               snd (* (klank (u/transpose [[111 1 0.8]
                                         [528 0.4 0.2]
                                         [588 0.4 0.2]
                                         [2552 0.05 0.2]
                                         [392 0.4 0.1]]) (white-noise))
                      (env-gen (env-perc 0.001 0.17) gate))]
           snd))

(demo 16 (let [gate (impulse (u/dt:kr 1 [2 2 8 2 2 8 2 2 16]) [0 1/16])
               release (u/dt:kr 1 [0.03 0.03 0.01])
               freq 330
               fenv (env-gen (envelope [0 (* 1.7 freq) freq] [0 release]) gate)]
           (* (sin-osc fenv) (env-gen (env-perc 0.0005 release)  gate))))

(demo 10 (let [gate (impulse 1)
               fenv (env-gen (envelope [0 1800 1400] [0 0.1]) gate)]
           (free-verb (ringz (* (white-noise) gate) fenv 0.04) 0.15 0.3 0.2)))

(demo 10 (u/m-map #(* (sin-osc (u/rg-lin (lf-noise0 32)
                                         %1 (* 1.2 %1)))
                      (clip2 (cubed (sin-osc 1/8 %2)) 0.5))
                  (range 2000 14000 1000)
                  (range 0 6 1/2)))

(demo 10 (let [trig (impulse 1 [0 1/2])
               snd (pulse (u/rg-lin (white-noise) 1500 2550))
               snd2 (lf-tri (u/rg-lin (white-noise) 1500 2550))]
           (* [snd snd2]
              (env-gen (env-perc 0.04 0.5) trig)
              (lf-pulse 10 0 (a2k (u/dt 7 [1 0.25]))))))

(defsound star
  {:period 32}
  (let [snd (* (saw (u/dt 1 [4000 4050]))
               (env-gen (env-perc 0.05 :t16) (u/throttle :-gate 64)))
        snd2 (comb-c snd 1/5 1/5 2)
        snd3 (comb-c snd2 1/3 1/3 3)]
    (+ snd (* 2/3 snd2) (* 1/3 snd3))))




(demo 10 (let [gate (impulse (u/rg-exp (lf-saw:kr -1/4 1) 1/2 20))]
           (* (+ (* 10 (rlpf (white-noise)
                             (env-gen (envelope [100 100 750 750] [0.02 0 1])
                                      gate)
                             0.4))
                 (* 1/5 (hpf (white-noise)
                             (env-gen (envelope [12000 0] [0.01])
                                      gate))))
              (env-gen (env-perc 0.01 0.15) gate))))

(defsound kan
  {:period 32
   :state {:initial (update-state {:count 0})
           :update update-state}}
  (splay (map #(ringz (* 4 %1 (white-noise)) %3 %2)
              [(impulse (u/rg-exp (lf-saw:kr 1/8) 1/10 8) 0.1)
               (impulse 2)
               (impulse (u/rg-exp (lf-saw:kr 1/4) 1/10 8))
               (impulse (u/rg-lin (lf-pulse:kr 1/6 0 7/8) 16 4))
               (impulse 1/8)]
              [0.5 0.1 0.2 0.01 1]
              [(!! (if b2 300 350)) 560 1200 (!! (if b0 2400 800)) 600])))

(defsound jump
  {:period 32
   :state {:initial (update-state {:count 0})
           :update update-state}}
  (let [gate (u/throttle :-gate [2 3])
        fenv (env-gen (envelope [0 0.25 1 8] [0 0.1 0.1]) gate)
        freq (* (u/dt 1/2 [400 50 200 800 1500]) fenv)
        delay-time (/ 1 (+ freq [(!! (if b1 0 300)) (!! (if b2 100 0))]))]
    (comb-l (* (white-noise) (env-gen (env-perc 0 0.00001) gate))
            0.1
            delay-time
            0.5)))



(demo 10 (u/m-map #(let [freq (x-line:kr 1 200 10)
                       snd (latch:ar (lf-pulse 12) (impulse freq))]
                   (ringz snd % 0.1))
                (repeatedly 10 #(u/rg-exp (lf-noise0 1/2) 500 8000))))
(l4/stop :star)

(defn up-down [{:keys [up?] :as m}]
  (-> m
      (assoc :up? (not up?))
      (update :freq #(-> (+ % (* (int (rand 2)) (if up? 1 -1) 171.4))
                        (max 400)
                        (min 3000)))))

(defsound thai2
  {:period 1
   :synth-option {:type :detune}
   :state {:initial {:freq 440}
           :update up-down}}
  (let [freq (!! freq)]
    (-> (sin-osc (lag-ud freq 0.01 0.02))
        (u/reduce-> (fn [acc x] (sin-osc (* dr acc freq x))) [3/2 1 3/2 2])
        (ringz (* (u/rg-lin (lf-pulse 16) 0.95 1) freq) 0.1)
        (hpf 5000)
        tanh)))

(l4/control :thai2 :vol {:dur 16 :to 1})

(l4/stop :thai2)

(l4/control :thai2 :vol {:dur 16 :to 0.1})

(l4/stop :thai2)

(l4/control :thai2 :vol {:dur 16 :to 1})


(l4/dump)
