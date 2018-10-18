(ns llll-work.unknown-colors-5
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)

(kill-server)
(l4/initialize {})
(l4/initialize {:osc-client false
                :osc-path "/hello"})
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

(def option {:swap-option {:switch-dur 8}
             :state {:initial (update-state {:count 0})
                     :update update-state}})
(def option-d (merge option {:synth-option {:type :detune}}))

(defsound happy option-d
  (splay (map (fn [phase]
                (let [trig (impulse (u/dt:kr 1 [4 12 32]) phase)
                      freq (* dr (u/dq trig (range 1000 10000 200)))
                      snd (* 2 (sin-osc (u/rg-lin (lf-pulse 8) (max 300 (/ freq 4)) freq))
                             (env-gen (env-perc 0.5 0) trig))]
                  (-> snd
                      (comb-n 1/4 1/4 1/4)
                      (comb-n 1/8 1/8 1)
                      (comb-n 1/8 1/8 2))))
              (u/n-range 0 1 8))))

(defsound delight option
  (let [trig (impulse 4)
        freq (u/dq trig (flatten [(u/n-range 400 1200 2)
                                  (u/n-range 2000 2300 8)]))
        env (env-gen (envelope [0,1,0.5,0.0] [0.02,0.1,0.1]) trig)
        filter (* 0.3 (rlpf (pulse freq 0.01) freq 0.01))
        filter (u/switch (u/rg-lin (lf-tri 1/32) 0 1) filter (distort (* 8 filter)))]
    (-> (* filter env)
        pan2
        (u/reduce-> (fn [acc x] (+ acc (comb-l acc 1/8 1/8 x)))
                    [0.2 0.2 0.1]))))

(defsound inharmony option-d
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
                    [(!! (if b2 1000 500)) (!! (if b0 800 3200))])
        (* 8) distort)))



(defsound sing option-d
  (let [gate (impulse 1/8)]
    (leak-dc
     (-> (u/m-map #(* %1
                      (ringz (* (lf-saw 32) (pink-noise))
                             (* dr %2 2)
                             (u/sin-r 0.27 0.01 0.18)))
                  (map #(* 8 % (u/sin-r 0.27)) [1 2/3 1/8])
                  (map (fn [x] (env-gen (envelope x  [0.3 0.02 0.8 0.08 0.3]) gate))
                       (u/transpose (map #(% formants) [:e :e    :i  :i :a :u]))))
         (free-verb 0.5 1)
         tanh))))

(defsound spin
  {:swap-option {:switch-dur 0}
   :period 64}
  [(section {:dur 7}
            (u/detune-rotate
             (sin-osc 2)
             (let [freqs (reduce (fn [acc x] (cons (* (first acc) x) acc))
                                 [300] (take 10 (cycle [1.08 1.21 2.1])))
                   gate (impulse (u/dt:kr 1 [4 8 8 1]))
                   f-env (env-gen (envelope [0 2 1 1 0] [0 0.1 0.4 0.5]) gate)]
               (tanh (* 12 (u/m-map #(let [freq (* f-env % dr)
                                           fm-freq (* 2/3 freq)
                                           fm-freq2 (* 1/8 fm-freq)]
                                       (sin-osc (* freq (sin-osc (* fm-freq (sin-osc fm-freq2))))))
                                    freqs))))))
   (section {:dur 1}
            (pan2 (let [freqs (reduce (fn [acc x] (cons (* (first acc) x) acc))
                                      [100] (take 8 (cycle [1.1 2.1 3.1])))
                        gate (impulse (u/dt:kr 1 [4 1]))
                        f-env (env-gen (envelope [0 2 1 1 0] [0 0.2 0.4 0.4]) gate)]
                    (tanh (* 12 (u/m-map #(let [freq (* f-env %)
                                                fm-freq (* 1/4 freq)
                                                fm-freq2 (* 1/8 fm-freq)]
                                            (sin-osc (* freq (sin-osc (* fm-freq (sin-osc fm-freq2))))))
                                         freqs))))))
   ])

(defsound shaving option-d
  (let [gate (impulse 1)
        f-env (env-gen (envelope [0 2 1 1 2 1] [0 0.08 0.1 0.08 0.2]) gate)]
    (u/switch (cubed (u/rg-lin (lf-saw 1/6) 0 1))
              (lf-tri (* f-env 8000))
              (tanh (* 32 (u/m-map #(let [freq (* f-env %)
                                          am-freq (* %2 freq)
                                          am-freq2 (* 1/5 am-freq)]
                                      (* (sin-osc freq) (sin-osc (* am-freq (sin-osc am-freq2)))))
                                   (reduce (fn [acc x] (cons (* (first acc) x) acc))
                                           [2800]
                                           (take 10 (cycle [1.5 1.1 1.1])))
                                   (cycle [1/4 1/8 1/16])))))))




(defsound light option-d
  (let [freq :-freq
        f-env (env-gen (envelope [0 2.3 0.4] [0.18 1]) (impulse (u/dt:kr 4 [0.8 8 1/2 1/2])))]
    (free-verb (tanh (u/m-map (fn [ratio] (u/m-map #(* 8 (sin-osc (* ratio % freq))
                                                       (u/sin-r f-env 0.1 1)
                                                       (/ 1 %))
                                                   (range 1 8 2)))
                              [1 10/3 5/3]))
               0.7 10)))


(defsound gone option
  (let [big-gate (impulse 1/20)
        freq-max (env-gen (envelope [10000 10000 1000 1000 10000] [2 5 3 6]) big-gate)
        freq-min (env-gen (envelope [100 100 800 2000 5000] [2 8 2 4]) big-gate)]
    (u/detune-rotate (sin-osc 0.01)
                     (-> (mix (repeatedly 30 #(let [gate (impulse 16 (rand 1))]
                                                (* (pulse (latch:ar (u/rg-exp (lf-noise1 8)
                                                                              (* dr freq-min)
                                                                              (* dr freq-max))
                                                                    gate))
                                                   (env-gen (env-perc 0.05 0.2) gate)))))
                         (free-verb 0.2 0.5)
                         (* 30)
                         tanh))))

(l4/control :gone :vol {:dur (* 8 16) :to 0})

(defsound evening option
  (splay (map #(let [gate (impulse 1/5 %2)
                     freq (latch (* (u/dt:kr 4 (u/n-range 440 880 4)) %) gate)]
                 (* (sin-osc (* freq ))
                    (u/eff (env-gen (envelope [0 0 0 1] [0 2 2]) gate) (u/sin-r 3.3 0 0.5))
                    (env-gen (envelope [0 1 1 0] [1 3 1]) gate)))
              (range 1 8)
              (reverse (u/n-range 1/4 1 8)))))

(defsound bell option
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


