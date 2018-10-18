(ns llll-work.shanghai
  (require [llll.core :as l4]
           [llll.clay.clay :as cl]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)

(kill-server)
(l4/initialize {})
(l4/finish)

(defn update-state [{:keys [count]}]
  (let [c (inc count)
        pairs (fn [prefix f]
                (map (fn [x] { (keyword (str prefix x)) (f x) })
                     (range 4)))]
    (-> {:count c}
        (into (pairs "b" #(bit-test c %)))
        (into (pairs "c" #(->> %
                               (bit-shift-right c)
                               (bit-and 2r0011)))))))

(def option {:state {:initial (update-state {:count 0})
                     :update update-state}
             :swap-option {:switch-fade-in 0
                           :switch-fade-out 0
                           :fade-out-dur 32}
             :period 64})

(def option-d (merge option {:synth-option {:type :detune}}))

(defsynth-l4 bang
  [freq 30 long 2]
  {:type :detune}
  (tanh (u/m-map #(* 2 (bpf (gray-noise) (* dr freq %) 1)
                (env-gen (env-perc 0.01 dur) :action FREE))
            [1 1/2])))

(def range-freq (partial  u/map-square 100 8000))

(defpattern bang
  {:table {:a (synth bang)}
   :swap-option {:switch-fade-in 0
                 :switch-fade-out 0
                 :fade-out-dur 64}
   :state {:initial {:n 4}
           :update (fn [m]
                     (update m :n #(if (< % 18) (inc %) 4)))}
   :period 64}
  (cl/->Clay
   (for [x (range 1 n)
         y (range x)]
     (cl/->Child (/ y x)
                 (+ (/ y x) (/ 1 x))
                 :a
                 {:freq (range-freq (/ (+ x y) 16))
                  :long 1
                  :vol 1/2}))))



(defsound waza
  option-d
  [(section
    {:dur 1}
    (-> (u/m-map #(let [gate (lf-pulse (* :f2 %) 1/2 (!! (if b0 1/12 3/4)))
                        freq (latch:ar (u/rg-exp (sin-osc 0.3) 100 1000) gate)]
                    (-> (sin-osc (* dr freq %2))
                        (u/reduce-> (fn [acc x] (sin-osc (* freq acc x))) [1 1/4 1/2])
                        (* gate)))
                 [1/3 1/4 1/7 2/3]
                 [10 30 32 15])
        (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000))
        (* 8)
        distort))
   (section
    {:dur 1}
    (-> (let [f-env (env-gen (envelope [(!! (if b0 2 1/2))
                                        (!! (if b1 2 1/2))
                                        (!! (if b2 2 1/2))]
                                       [(* 1/4 dur) (* 3/4 dur)]))
              freq 1200]
          (u/m-map #(u/switch (line 1 0 dur)
                              (rlpf (white-noise) (* f-env dr %) 0.01)
                              (sin-osc (* f-env dr %)))
                   [freq (* freq 3/2) (* freq (!! (if b0 4 1/4)))]))
        (u/reduce-> (fn [acc x] (comb-l acc x x 0.2)) [0.01 0.03 0.008])
        (u/switch-> (!! b2) (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000)))
        (u/switch-> (!! b1) (moog-ff (u/rg-exp (u/m-map lf-pulse [1.3 0.2 0.5]) 1000 3000)))
        (* 8)
        distort))])

(defsound waza
  (merge option-d {:period 64})
  [(section
    {:dur 1}
    (-> (u/m-map #(let [gate (lf-pulse (* :f2 %) 1/2 (!! (if b0 1/12 3/4)))
                        freq (latch:ar (u/rg-exp (sin-osc 0.3) 100 1000) gate)]
                    (-> (sin-osc (* dr freq %2))
                        (u/reduce-> (fn [acc x] (sin-osc (* freq acc x))) [1 1/4 1/2])
                        (* gate)))
                 [1/3 1/4 1/7 2/3]
                 [10 30 32 15])
        (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000))
        (* 8)
        distort))
   (section {:dur 1}
            (let [snd (rlpf (white-noise) (* dr [1200 1800]) 0.0001)
                  freq (u/rg-exp (u/m-map lf-pulse [(!! (if b0 10.3 2.8))
                                                    (!! (if b1 0.8 0.6))]) 80 1800)]
              (-> snd
                  (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [1/2 1 2])
                  (free-verb (!! (if b1 1 0.3)) (!! (if b2 1 0.5)))
                  (* 8) distort)))])


(defsound chill
  {:swap-option {:switch-fade-in 8, :switch-fade-out 4, :fade-out-dur 32},
   :period 64,
   :synth-option {:type :detune}}
  (let [snd (bpf (white-noise) (* dr [1000 2000 10000]) (u/rg-exp (lf-saw -1/10) 1 0.01))]
    (-> snd
        (free-verb 1 0.1)
        (comb-l 1/8 1/8 1.2)
        mix
        tanh)))

(defsound wakare option
  (-> (let [[x y] (splay (* (reductions (fn [acc x] (+ acc (* 1/2 (moog-ff (delay-n acc 0.2 0.2)
                                                                           x 3.999))))
                                        (white-noise)
                                        (take 8 (iterate #(* 1.5 %) 200)))
                            (map #(env-gen (env-perc 0.05 0.5) (impulse 2 %))
                                 (u/n-range 0 1 8))))]
        (rotate2 x y (sin-osc 0.1)))
      (lpf (u/sin-r 0.07 500 1800))
      (* (u/sin-r 0.02 1 10))
      (distort)))

(defsound nami
  {:swap-option {:switch-fade-in 4, :switch-fade-out 4, :fade-out-dur 32}, :period 64}
  (-> (rotate2 (rlpf (white-noise) (u/rg-exp (sin-osc 0.057) 200 1800))
               (rlpf (white-noise) (u/rg-exp (sin-osc 0.03) 100 1000))
               (sin-osc (u/rg-exp (sin-osc 0.008) 0.03 1)))
      (* 2)))

(defsound waza
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 128}
  [(section {:dur 3}
            (let [snd (u/m-map #(let [gate (lf-pulse %2 1/2 1/4)
                                      freq (latch:ar (u/rg-exp (sin-osc (!! (if b0 8.3 0.3)))
                                                               100 2000) gate)]
                                  (* (sin-osc (* dr % freq)) gate))
                               [5 18 28 4 8]
                               [:f6 :f8 :f4 :f7 :f5])]
              (-> snd
                  (u/reduce-> (fn [acc [x y]] (ringz (comb-l acc x x 0.3)
                                                     y 0.01))
                              [0.01 0.02]
                              [(!! (if b2 1000 500)) (!! (if b0 800 3200)) 10])
                  (* 8) distort)))
   (section {:dur 0}
    (let [f-env (env-gen (envelope [0 6 1] [(!! (if b0 0.05 0.4)) (!! (if b2 0.1 0.5))]))
          freq (* f-env (u/rg-exp (lf-saw (!! (if b2 -4 16))) 800 (!! (if b0 1600 520))))
          snd (u/switch (lf-pulse 8 0 1/4) (sin-osc (* dr freq))
                        (bpf (gray-noise) (* dr freq)))]
      (-> snd
          (u/switch-> (line 1 0 dur)
                      (u/reduce-> (fn [acc x] (sin-osc (* acc x freq)))
                                  [1 1/4 (!! (if b0 1/2 1/4)) 3]))
          (u/reduce-> (fn [acc x] (comb-l acc x x 0.3)) [0.01 0.02])
          (* 8) distort)))])

(defsound waza
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :swap-option {:switch-fade-in 0, :switch-fade-out 0, :fade-out-dur 128}
   :period 64}
  [(section {:dur 7}
            (let [freq (u/rg-exp (u/m-map lf-saw [2.3 3.8 2.8])
                                 (!! (if b1 100 800))
                                 (!! (if b0 4880 2800)))
                  snd (mix (sin-osc (* dr [1200 3200 640])))]
              (-> snd
                  (u/switch-> (!! (= c0 3))
                              (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [1 3 1/2 4]))
                  (u/switch-> (line 1 0 dur)
                              (u/reduce-> (fn [acc x] (ringz acc x dur)) [(* 2 freq) freq]))
                  (free-verb 1 10)
                  (* 8) distort)))])

