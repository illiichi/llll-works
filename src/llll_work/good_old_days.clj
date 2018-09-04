(ns llll-works.good-old-days
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]
       [music.helper]))

(connect-external-server "localhost" 57110)
(kill-server)
(l4/finish)

(l4/initialize {})

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

;; 2017.01.31
(defsound test option
  (+ (splay
      (map
       (fn [gate freq]
         (let [env (rg-lin (sin-osc (env-gen (envelope [0 30 3 1] [0.3 0.2 1]) gate))
                           0.5 1)
               f-env (rg-lin (sin-osc (env-gen (envelope [0 30 3 1] [0.3 0.2 1]) gate))
                             0.95 1)]
           (* env (sin-osc (* freq f-env)) (env-gen (envelope [0 1 1 0] [2 1 4]) gate))))
       [(impulse 1/8)
        (impulse 1/3 1/2)
        (impulse 1)
        (impulse 1/4 1/3)]
       [6000 800 5000 300]))
     (* (sin-osc 300) (saw [5000 5010])
        (env-gen (env-perc 0.03 2) (pattern (impulse 1 [1/64 2/64])
                                            [[1 0 0 0]
                                             [0 0 1 1]
                                             [0 0 0 0]
                                             [0 1 0 0]])))
     (* (rg-exp (lf-noise1 1/3) 0.4 1)
        (rlpf (brown-noise) (rg-exp (lf-noise0 3.3) 50 180) 0.2))))


;; 2017.02.02
(defsound test option
  (+ (let [gate (impulse 1)
           freq (+ (dq gate [440 500 550 500])
                   (rg-lin (lf-pulse 1/4) 100 1000))
           env (env-gen (envelope [0 1 1 0] [0.1 0.2 0.5]) gate)]
       (* (sin-osc (* freq (m-map lf-pulse [3.2 1.3 8.3 1/3])))
          env 64
          (m-map (fn [idx]
                   (* (sin-r 3.2 0.1 1)
                      (/ 1 idx) (bpf (white-noise) (* freq idx) 0.01)))
                 (range 1 16 3))))
     (* 1/2 (bpf (saw [100 120]) (rg-exp (lf-tri 1/5 [-1 0]) 20 [1000 3000]) 1)
        (lf-pulse (dt:kr 5 [4 32])))
     (* 1/2 (saw (dt:kr 1/4 [180 38 20 14])) (gray-noise) (lf-pulse 1/2 1/2))))

;; 2017.02.03
(defsound test option
  (+ (let [gate (lf-pulse (dt:kr 1 [8 8 8 2]) 0 1/8)]
       (* (bpf (white-noise)
               [(dq gate [1000 8000 2000 2500])
                (dq gate [300 250])]
               [1 0.1])
          gate))
     (map #(mix (let [gate (impulse 1/8)]
                  (* 1/2 (rlpf (pulse (* (range 1 9) (dq gate [200 150 400])
                                         (sin-r 3.3 0.99 (+ 1 %))))
                               (env-gen (envelope [0 8000 10] [0 8] EXP) gate))
                     (env-gen (envelope [0 1 1 0] [3 1 3.5]) gate))))
          [-0.01 0.01])))

;; 2017.02.03(beautiful)
(defsound test option
  (let [freq-base (lin-exp (m-map lf-pulse [1/3 2.8 1.4 9.2])
                           0 1 800 3200)]
    (+ (splay (map #(let [gate (impulse 2 %)
                          freq (* (latch:ar freq-base gate) %2)]
                      (* (sin-osc freq)
                         (env-gen (env-perc 0.01 0.3) gate)))
                   (n-range 0 1 8)
                   (n-range 1 4 8)))
       (* (rg-exp (lf-noise0 8) 1/16 1/4)
          (ringz (* (impulse 1) (white-noise)) (* 1/4 freq-base) 0.8))
       (* 2 (cubed (pink-noise)) (lf-pulse (dt:kr 1 [16 1]))))))


;; 2017.02.04
(defsound test option
  (let [gate (dust:kr 30)]
    (+ (* 1/4 (tanh (* 16 (saw (sin-r 0.8 80 80.5)) (saw (sin-r 2.4 40 41))
                       (env-gen (env-perc 0.02 0.2) gate))))
       (m-map #(let [freq (* %2 (rg-lin (lf-pulse 16 %) 1 1.1))]
                 (* (saw freq) (env-gen (env-perc 0.08 2) (dust:kr 0.2))))
              (reverse (n-range 1/2 1 8))
              (repeatedly 8 #(ranged-rand 4000 12000))))))

;; 2017.02.04
(defsound test option
  (+ (splay (map #(let [gate (* (lf-pulse 1/2 %2)
                                (lf-pulse (sin-r:kr 1.2 1/4 8))
                                (impulse 16))]
                    (* (sin-osc %1) (env-gen (env-perc 0.01 0.1) gate)))
                 (shuffle (take 12 (iterate #(* % 1.25) 440)))
                 (n-range 0 1 12)))
     (* (lf-saw 8) (lf-saw 1/4 -1)
        (let [f-env (env-gen (envelope [0 3 1] [0 0.18]) (impulse 1))
              freq (rg-exp (lf-saw (dt:kr 4 [1/4 -1/2 1 8]) -1) 300 10000)]
          (bpf (brown-noise) (* f-env freq) (line 0.8 0.3 16))))))

;; 2017.02.05
(defsound test option-d
  (let [gate (impulse 4 dr)
        freq (+ (rg-exp (lf-pulse 4) 400 500)
                (* (rg-lin (lf-tri 2) 100 1800) (clip:ar (lf-tri 1/8 -1) 0 1)))]
    (+ (* (rlpf (saw 30) freq 0.4) (env-gen (env-perc 0.02 0.5) gate))
       (* (x-line 1/32 1/2 16) (rlpf (gray-noise) (* 3 freq) 0.8)
          (env-gen (envelope [0 1 0] [0.3 0.01])
                   (coin-gate 0.8 gate))))))

;; 2017.02.27
(defsound test option
   (let [gate (pattern (impulse 4) [[1 0 0 0]
                                      [0 0 0 0]
                                      [1 0 1 0]
                                      [1 0 0 0]])
           freq (* (lin-exp (+ (m-map sin-osc [0.4 1/4 1.3]) (m-map lf-pulse [8 5.4 18.3]))
                            -1 2 150 1200)
                   (env-gen (envelope [1 1 6] [8 8])))]
       (+ (* (sin-osc freq)
             (env-gen (envelope [0 1 1 0] [0.2 0.1 0.4]) gate))
          (* (sin-osc 4) (sin-osc 0.12) (rlpf (brown-noise) (* freq 4) 0.2)
             (env-gen (env-perc 0.2 2) gate))
          (free-verb (let [speed (env-gen (envelope [1.5 1.5 10] [8 8] EXP))]
                       (* (line 0 1 12) (sin-osc speed) (blip 30)))
                     (env-gen (envelope [1 1 0.5] [8 8]))))))

(defsound test option
  (let [gate (impulse 2)
        freq (latch:ar (rg-exp (m-map lf-saw [1 4 3.2 2.8]) 220 1200) gate)]
    (m-map #(* (sin-osc (* freq %))
               (env-gen (env-perc (* 0.02 %2) (* 0.1 %)) gate))
           (range 1 12)
           (cycle (range 1 3)))))

(defsound test option-d
  (let [gate (impulse 1)
        freq [440 860 2902.5]
        rq (env-gen (envelope [0 4 1] [0 0.025] EXP) gate)]
    (mix (* (sin-osc (* freq (rg-lin (sin-osc freq) rq 1)))
            (env-gen (env-perc 0.05 0.5) gate)))))

(defsound test option-d
  (m-map #(let [gate (impulse 1 %)]
            (* 4 (rlpf (gray-noise) %3 0.1)
               (env-gen (env-perc 0 %2) gate)))
         (n-range 0 1 8)
         [0.1 0.1 0.3 0.1 0.2 0.1 0.5 0.1 0.1 0.2]
         [400 360 450 400 400 400 450 310 310 350]))

(defsound test option-d
  (let [gate (impulse 8)
        release (dq gate [0.04 0.01 0.01 0.01])
        freq (dq gate [8000 1000 2000 3000])]
    (* (hpf (white-noise) freq)
       (env-gen (env-perc 1e-6 release) gate))))

(defsound test
  {:swap-option {:switch-dur 4}, :synth-option {:type :detune}}
  (let [gate (pattern (impulse 16 dr) [[1 0 0 0 1 1 0 0]
                                       [1 0 0 0 1 1 0 0]
                                       [1 1 1 0 1 1 1 0]
                                       [1 1 0 0 1 0 0 0]])
        freq (dq gate [8000 5000 4000])]
    (* (rlpf (white-noise) freq 0.5) (env-gen (env-perc 1e-3 0.24) gate))))

;; 2017.03.08
(defsound test option
  (+ (splay (map #(* (env-gen (env-perc 0.05 1.5) (impulse 1/4 (* 1/10 %)))
                     (sin-osc (* % (rg-exp (m-map (fn [i]
                                                    (lf-pulse i (* 1/10 %)))
                                                  [3.7 0.3 0.7])
                                           440 1200)
                                 (sin-r 3.3 0.99 1))))
                 (range 1 8)))
     (let [gate (impulse 8)
           f-env (env-gen (envelope [0 4 1] [0 0.08]) gate)
           env (env-gen (env-perc 0.04 0.1) gate)]
       (* (rhpf (white-noise) (* f-env (dq gate [8000 5000])) 0.3)
          env (sin-r 0.3 1/16 1/2)))
     (m-map #(let [gate (impulse 1/2 %)
                   freq (* %3 (env-gen (envelope [0 3 1] [0 0.2]) gate))]
               (* 1/16 (rlpf (saw 60) freq 0.01) (env-gen (env-perc 1e-4 %2) gate)))
            [0 0.5 0.6]
            [1.3 0.8 0.4]
            [8000 12000 1400])
     ))

(defsound test option
  (+ (splay (map #(* (env-gen (env-perc 0.05 1.5) (impulse 1/4 (* 1/10 %)))
                     (sin-osc (* % (rg-exp (m-map (fn [i]
                                                    (lf-pulse i (* 1/10 %)))
                                                  [(!! (if b0 0.7 3.7)) 0.3 0.7])
                                           440 (!! (inc c2) 1200))
                                 (sin-r 3.3 0.99 1))))
                 (range 1 8)))
     ;; (let [gate (impulse 8)
     ;;       f-env (env-gen (envelope [0 4 1] [0 0.08]) gate)
     ;;       env (env-gen (env-perc 0.04 0.1) gate)]
     ;;   (* (rhpf (white-noise) (* f-env (dq gate [8000 5000])) 0.3)
     ;;      env (sin-r 0.3 1/16 1/2)))
     (m-map #(let [gate (impulse 1/2 %)
                   freq (* %3 (env-gen (envelope [0 3 1] [0 0.2]) gate))]
               (* 1/16 (rlpf (saw 60) freq 0.01) (env-gen (env-perc 1e-4 %2) gate)))
            [0 0.5 0.6]
            [1.3 0.8 0.4]
            [(!! (if b2 5000 8000))
             (!! (if b2 500 12000))
             (!! (if b2 800 1400))])
     ))



;; 2017.03.09
(defsound test option
  (+ (apply m-map #(let [gate (pattern (impulse %) %2)]
                     (* %3 (env-gen %4 gate)))
            (transpose [[1/4 [0 1 1 1] (gray-noise) (env-perc 0.05 0.8)]
                        [1 [0 0 0 1] (brown-noise) (env-perc 0.05 0.5)]
                        [4 [[0 0 0 0]
                            [1 1 1 1]
                            [0 0 0 0]
                            [0 0 0 0]] (hpf (brown-noise) 4000) (env-perc 0.05 0.5)]
                        [16 [[1 1 1 1]
                             [1 1 1 1]
                             [0 0 0 0]] (* 1/4 (pink-noise)) (env-perc 0.05 1e-3)]
                        [8 [1 1 1 0] (brown-noise) (env-perc 1e-4 0.08)]]))
     (let [gate
           (pattern (impulse 2) [[1 0 0 0]
                                 [1 0 0 0]
                                 [1 0 1 0]
                                 [1 1 1 0]])
           vib (sin-r 3.3 (env-gen (envelope [0 0.99 0.95] [0 0.8]) gate) 1)
           freq (dq gate [120 90 60 45 45 60 90 ])]
       (* (+ (sin-osc (* vib freq))
             (* 1/2 (lf-tri (* 1/2 freq))))
          (env-gen (envelope [0 1 1 0] [0.05 0.8 0.8]) gate)))))

;; 2017.03.10
(defsound test option
  (let [gate (impulse 1/2)
        gate2 (impulse 8)
        freq (dt 4 [120 60])
        f-env (env-gen (envelope [0 3/2 1] [0 0.08]) gate)
        env  (env-gen (envelope [0 1 1 0] [0.05 0.4 1]) gate)
        env2 (env-gen (env-perc 0.01 0.2) gate)
        env3 (env-gen (env-perc 1 1) gate)]
    (+ (* env (sin-osc (* f-env freq)))
       (* env3 (/ 1 192) (lf-tri (* freq 4)))
       (* env2 (sin-osc (* 0.75 freq)))
       (apply m-map #(let [gate (pattern gate2 %)]
                       (* (env-gen (env-perc 0 %3) gate) (rlpf (white-noise) %2  0.1)))
              (transpose [[[1 1 1 1 0 0 0 0] 8000 0.01]
                          [[ 0 0 0 0 1 1 1 1] 6000 0.08]
                          [[ 0 0 0 0 0 1 0 0] 3000 0.2]
                          [[ 0 0 0 1 0 0 0 1] 3800 0.1]
                          [[ 0 0 1 1 1 0 0 0] 1200 0.4]
                          [[[ 0 0 0 0 0 0 0 0]
                            [ 1 0 0 0 0 0 0 0]] 820  1]
                          [[[ 0 0 0 0 0 0 0 0]
                            [ 0 0 1 0 0 1 0 0]] 1820  1]])))))
;; 2017.03.10
(defsound test option
  (+ (let [gate (impulse 1/2 1/4)]
       (* (lpf (brown-noise) 300) (env-gen (env-perc 0.05 0.5) gate)))
     (let [gate (impulse 8)
           freq (dq gate [120 90])
           f-env (env-gen (envelope [0 7 1] [0 0.02]) gate)]
       (switch (lf-pulse 1)
               (* (sin-osc (* f-env freq))
                  (env-gen (env-perc 0.1 0.02) gate))
               (sin-osc (rg-exp (lf-saw -1) 20 40))))
     (out 1 (+ (let [gate (impulse (dt:kr 4 [4 16]))
                     freq (latch:ar (rg-lin (lf-saw 1) 40 120) gate)]
                 (sin-osc freq))
               (let [gate (impulse (dt:kr 8 [4 16]))
                     freq (dq gate [60 40])]
                 (* (env-gen (env-perc 1e-4 0.08) gate) (lpf (gray-noise) freq)))))))
;; 2017.03.11
(defsound test option
  [(* (m-map #(* 4 (env-gen (envelope [0 1 1 0] [1 1 1]) (impulse 1/5 %))
                 (sin-osc %2) (sin-osc 110))
             (n-range 0 1 5)
             (n-range 10 50 5)))
   (switch (rg-lin (lf-saw 1/5 -1) 0 1)
           (let [gate (impulse (dt:kr 2 [[2 4]]))
                 note (+ (* 5 (lf-pulse 3.3))
                         (rg-lin (lf-saw 1/5 -1) 20 40)
                         (stepper (impulse 1/5) 0 0 12 3))
                 freq (midicps note)]
             (sin-osc freq))
           (let [gate (impulse 4 1/2)
                 freq (env-gen (envelope [1e-5 80 20] [0 1/8] EXP) gate)]
             (* (sin-osc freq) (env-gen (env-perc 1e-4 0.1) gate))))])

;; 2017.03.14
(defsound test option
  (let [max-ratio (env-gen (envelope [1.25 1.25 3 1.02 8] [6 2 2 6]))]
    (+ (m-map #(let [freq (latch:ar (sin-r 0.4 % (* max-ratio %))
                                    (dust max-ratio))]
                 (* (sin-r 0.3 0.1 0.5)
                    (sin-osc freq) (sin-osc (* (sin-r 0.3 1/2 1/16) freq))))
              [300 430 480 520 610 1400])
       (let [gate (pattern (impulse 2 1/2) [1 1 1 0])
             freq (dq gate [320 320 550])
             f-env (env-gen (envelope [0 8 1] [0 0.005]) gate)]
         (* 3/2 (bpf (brown-noise) (* f-env freq) 0.1) (env-gen (env-perc 0.01 0.2) gate)))
       (let [gate (pattern (impulse 2 1/2) [0 0 0 1])]
         (+ (* (lf-pulse 8 0 (dq gate [1 1/8])) (white-noise)
               (env-gen (env-perc 0.05 0.5) gate))
            (dust (* 10 max-ratio)))))))
;; 2017.03.16
(defsound test option
  (+ (let [snd (mix (repeatedly 4 #(white-noise)))]
       (switch (lf-tri (env-gen (envelope [1/4 1/4 8] [5 5])))
               (switch (lf-pulse 8) (hpf (white-noise) 1000) (pink-noise))
               (+ (rlpf snd
                        (rg-exp (sin-osc (sin-r 1/3 1/3 3.3)) 300 2000))
                  snd)))
     (* 2 (lf-noise0 80) (env-gen (env-perc 1e-8 0.12) (dust:kr 10)))))

;; 2017.03.17
(defsound test option-d
  (let [gate (impulse 1 dr)
        gate-q (impulse 8)
        gate2 (pattern gate [1 0 0])
        freq (midicps (dq gate [[40 44 47]
                                [40 44 47]
                                [38 40 47]]))]
    (+ (* (+ (sin-osc (* freq (latch:ar (env-gen (envelope [0 1 1 8] [0 0.1 0.5]) gate)
                                        (impulse 8))
                         (rg-lin (lf-noise0 16) 0.9 1.5)))
             (* 1/4 (saw (* 4 freq))))
          (env-gen (env-perc 0.05 1) gate))
       (switch (lf-pulse 1)
               (* (gray-noise) (env-gen (env-perc 0.01 0) gate-q))
               (let [freq2 (* freq (dq gate-q (n-range 1.8 1 8)))]
                 (* (sin-osc freq2)
                    (env-gen (env-perc 0.01 0.1) gate-q))))
       (* (brown-noise)
          (env-gen (envelope [0 1 1 0] [0 0.3 0]) gate2)
          (env-gen (env-perc 0.05 0.5) gate2))
       (switch (lf-pulse 1/4 0 (dt:kr 2 [1 1 3/4 0]))
               (* 1/4 (pulse (* freq 1/4 (sin-r 3.3 0.98 1)))
                  (env-gen (envelope [0 1 1 0] [1e-4 0.5 0.8]) gate))
               (* (rlpf (lf-tri (* freq 1/4))
                        (* freq (env-gen (envelope [1e-6 8 1e-6] [0 1] EXP) gate))
                        0.1))))))

;; 2017.03.24
(defsound test option-d
  (let [gate (m-map impulse [1 1.2 2.1 1/4 1/7])
        base (m-map lf-pulse [1 1.2 2.1 1/4 1/7])
        freq (lin-exp base 0 1 200 8000)]
    (+ (* (sin-r 1/5 0.1 0.5)
          (sin-r 1/2 0.4 1)(sin-osc freq))
       (* (rlpf (white-noise) (* dr (dq gate [1200 700 700 700])) 0.1)
          (env-gen (env-perc 0.0 0.08) gate))
       (* (bpf (brown-noise) (latch:ar freq gate)) (sin-r 1/7 0.1 0.5))
       (* (rlpf (saw 80) (lin-exp (+ (sin-osc 2.1)
                                     (lf-pulse 1.2)
                                     (lf-pulse 1)) -1 3 100 8000) 0.8)
          (env-gen (env-perc 0.05 0.5) (pattern gate [[1 0 0 0]
                                                      [1 0 0 0]
                                                      [1 0 0 0]
                                                      [1 1 0 0]]))))))

(defsound test option-d
  (let [arr (map #(* dr %) [400 900 1300 1800 3600])]
    (mix (* (rlpf (white-noise) arr 0.05)
            (env-gen (env-perc 0.05 0.5) (impulse 1 (n-range 1e-10 1/4 (count arr))))))))



(defsynth-l4 test-li []
  (* (mix (moog-ff (* (pulse 120)
                      (sin-osc (rg-lin (white-noise) 30 60)))
                   [478 2333 3332 3866] 3.99))
     (env-gen (envelope [0 1 1 0] [0.024 0.1 0.04]) :action FREE)))

(defsynth-l4 test-li2 []
  (* (+ (mix (* (let [freq [2333 3332 3866]
                      vol [1 1/4 1/4 1/6]
                      snd (sin-osc (* freq
                                      (sin-osc (* 1/16 freq (sin-r 17.2 0.98 1)))))]
                  (* vol (moog-ff snd freq 3.89)))
                (env-gen (env-perc 0.001 0.08) (impulse 14))))
        (let [f0 478]
          (rlpf (* 1/8 (sin-osc (* f0 (sin-osc (* 1/4 f0)))))
                f0 0.2)))
     (env-gen (envelope [0 1 1 0] [0.024 0.1 0.04]) :action FREE)))

(defsynth-l4 test-li3 []
  (* (let [gate (impulse 125)
              f0 125]
          (+
           (* 1/16 (sin-osc (sin-r 38.3 220 230)))
           (* 1/16 (lpf (pulse (sin-r 88.3 338 478)) 500))
           (* 1/4 (sin-r 10 1/8 1/4) (sin-osc 2333)
              (env-gen (envelope [0 1 1 0] [0.02 0.059  0.055] EXP))
              (env-gen (envelope [0 0 1 0] [0.002 0.001 0.0055]) gate))
           (* 0.3 (sin-r 10 1/3 1/2) (sin-osc 3266)
              (env-gen (envelope [0 0 1 0] [0.002 0.0016 0.004]) gate))
           (* 0.25 (sin-r 10 1/3 1/2) (sin-osc 3866)
              (env-gen (envelope [0 0 1 0] [0.002 0.0016 0.004]) gate))
           (* 1/4 (sin-r 10 1/3 1/2) (m-map pulse [5500 7150 8500])
              (env-gen (envelope [0 0 1 0] [0.002 0.0016 0.004]) gate))))
        (env-gen (envelope [0 1 1 0] [0.024 0.1 0.04]) :action FREE)))

(defsynth-l4 test-li4 []
  (* 2 (+ (sin-osc 125)
          (mix (* (clip:ar (lf-saw -128) 0 0.5)
                  (repeatedly 4 #(rg-lin (lf-noise1 3.3) 0.5 1))
                  [0.5 0.7 0.8 0.3 0.3]
                  (sin-osc (* (rg-lin (lf-noise1 12.3) 0.95 1)
                              [2222 3342 3745 7683 13420]))))
          (* 1/6 (m-map #(* (/ 1 %) (bpf (white-noise) (* % 125 (sin-r 3.2 0.95 1))
                                         0.01))
                        (range 1 18 4))))
     (env-gen (envelope [0 1 1 0] [0.05 0.06 0.05]) :action FREE)))


(defsynth-l4 test-li5 []
  (* (+ (m-map #(let [f0 %]
                  (* %2 3
                     %3
                     (lf-tri (* f0
                                (sin-r 3.3 0.96 1)
                                (rg-lin (white-noise) 0.98 1)))))
               [125 2333 3266 3866]
               [1 0.5 0.7 0.8]
               [1
                (lf-pulse 125 0 1/8)
                (lf-pulse 125 0 1/8)
                (lf-pulse 125 0 1/8)])
        (* 1/16 (m-map #(rlpf (white-noise) % 0.01)
                       [125 2333 3266 3866 7683 13420])
           (env-gen (envelope [0 1 1 0] [0.05 0.06 0.05]))))
     (env-gen (envelope [0 1 1 0] [0.05 0.06 0.05]) :action FREE)))

(defsynth-l4 test-li6 []
  (* 4 (+ (* 1/16 (lpf (lf-tri 125) 400)
             (env-gen (envelope [0 1 1 0] [0.05 0.06 0.05]) :action FREE))
          (* 16 (mix (* [0.3 0.7 0.6] (bpf (white-noise) [2333 3266 3866] 0.005)))
             (clip:ar (cubed (lf-saw -125)) 0.3 1)
             (env-gen (envelope [0 1 1 0] [0.05 0.06 0.05]) :action FREE))
          (* (env-gen (env-perc 0.01 0.15))
             (rg-lin (lf-tri 245) 0.5 1)
             (mix (bpf (white-noise) [4738 5695] 0.01))))))
(defsynth-l4 test-li7 []
  (let [f4 (env-gen (envelope [3300 3900] [0.05]))]
    (+ (* 16 (+ (mix (* [1 0.6 0.2] (bpf (white-noise) [f4 520 14451] 0.01)))
                (* (lag-ud (impulse 31.25) 1e-9 0.002)
                   (mix (* [1 0.6 0.7] (bpf (white-noise) [70 2357 f4] 0.1)))))
          (env-gen (envelope [0 1 1 0] [0.09 0.06 0.05]) :action FREE))
       (* (lpf (saw (rg-lin (white-noise) 80 130)) (* 2 125))
          (env-gen (envelope [0 1 1 0] [0.09 0.06 0.05])))
       (* 1/4 (+ (sin-osc (rg-lin (lf-noise1 100) 100 120))
                 (bpf (white-noise) 2585 0.2))
          (env-gen (envelope [0 1 1 0] [1e-3 0.01 0.008]))))))


;; 2017.03.26 (like)
(defsound test-2 option
  (let [freq 180
        s (env-gen (envelope [1/8 1/8 1/2 4 1/3000] [8 6 2 16] EXP) (impulse 1/32))]
    (splay (map #(* (sin-osc (* freq %))
                    (lag-ud (lf-pulse s (/ % 10))
                            0.3 1))
                (range 1 10)))))

;; 2017.03.26
(defsound test option
  (let [gate (impulse (dt:kr 1 [3 5 8]))
        freq (dq gate [[8000 2000 1000]
                       [4000 2000 1000 1000 1000]
                       [8000 2000 1000 1500 1000 1500 1000 3000]])]
    (* (bpf (brown-noise) freq 0.1)
       (env-gen (env-perc 0.01 0.1) gate))))

;; 2017.04.09
(defsound test option
  (let [freq (dt 1 [[880 880 1320]
                    [880 880 660]])]
    (+ (m-map #(let [gate (impulse 1 (* 1/16 %))]
                 (+ (* (sin-osc (* % freq))
                       (env-gen (env-perc 0.2 0.5) gate))
                    (* (saw (* freq % 2))
                       (lf-noise0 16)
                       (env-gen (env-perc 0.4 0.4) gate))))
              (range 1 8))
       (* 1/2 (lf-pulse 1/2 1/2) (lf-pulse (sin-r:kr 2 2 16)) (gray-noise))
       (let [gate (impulse 1/2)
             f-env (env-gen (envelope [0 8 1 0] [0 0.1 1]) gate)]
         (* (rlpf (saw 30) (* freq 1/2 f-env) 0.4)
            (env-gen (env-perc 0.01 1) gate))))))

;; 2017.04.10
(defsound test option
  (+ (let [gate (impulse 16)
           freqs [(dq (pattern gate [1 0 0 0]) [400 600])
                  (latch:ar (rg-exp (sin-osc 1/3) 200 8000) gate)
                  (dq gate [[4800 2400]
                            [4800 2400]
                            [4800 6400]])]]
       (* (mix (rlpf (white-noise) freqs 0.1))
          (env-gen (env-perc 1e-5 0.05) gate)))
     (splay (map #(let [gate (impulse 1/4 (/ % 8))
                        freq (dq gate [900 500 700 2300])]
                    (* (sin-r 3.2 0.9 1) (sin-osc (+ freq (* % 50)))
                       (env-gen (envelope [0 1 1 0] [0.2 0.5 1]) gate)))
                 (range 1 8)))
     (let [freq (switch (lf-pulse 1/4)
                        (rg-exp (m-map lf-tri [1/3 3/2 1.2]) 1000 4000)
                        (rg-exp (lf-pulse 2) 2000 6000))]
       (* (rg-lin (lf-pulse 1/4) 1 1/8) (rlpf (brown-noise) freq 0.1)))))

;; 2017.04.11 (like)
(defsound test option
  (+ (let [gate (impulse 8)
           freq [(dq gate [1200 300 1800 1500])
                 (dq gate [600 700 2200])]
           width (rg-lin (sin-osc 1.2) 0.1 0.5)]
       (+ (* (rlpf (white-noise) freq)
             (rg-lin (lf-saw 1/8 -1) 1 4)
             (env-gen (env-perc 0 0.02) gate))
          (* 1/16 (lf-pulse 8) (+ (sin-osc freq)
                                  (lf-tri (* 4 freq) width)))
          (* (pulse freq width) (env-gen (env-perc 0.08 0.5)
                                         (impulse (dq (impulse 1)
                                                      [[1/4 1/2 1/4 1/2]
                                                       [1/4 1/2 1/4 1/2]
                                                       [1/4 1/2 1/4 1/2]
                                                       1/8 8]) )))))))

;; 2017.04.13
(defsound test option-d
  (let [freq (map #(lin-lin:kr (clip2 (sin-osc:kr 1/8 %) 0.2) -0.2 0.2 350 (* 700 %2))
                  (n-range 0 (* 2 3.141) 20)
                  (n-range 1 3 20))]
    (mix (+ (sin-osc freq)
            (* (lf-pulse 8)
               (moog-ff (white-noise)
                        (* (dt 2 [3/2 3/2 5/2 4 7/2]) freq)
                        3.9))
            (* 1/16 (lf-pulse 32 0 (sin-r:kr 1/4 0.01 0.5)) (rhpf (white-noise) freq))))))


;; 2017.04.15 (low)
(defsound test option-d
  (let [dur 1/2
        gate (impulse (/ 1 dur))
        period (impulse (/ 1 (* 4 dur)))
        freq (env-gen (envelope [0 100 100 60 180] [0 (* 3 dur) 0 (* dur)])
                      period)
        am (env-gen (envelope [0 2 2 1 4] [0 (* 3 dur) 0 (* dur)])
                    period)
        env (env-gen (env-perc 0.01 dur) gate)]
    (+
     (* (lpf (saw 25) 200) (env-gen (env-perc 0.3 2) (impulse 1/4 1/2)))
     (* (+ (* (sin-osc (/ freq am)) (sin-osc freq))
           (sin-osc freq))
        env)
     (let [freq (sin-r 0.3 40 100)]
       (* (sin-r 0.1 1/8 1)  (sin-osc (* (sin-osc 30) freq)))))))

;; 2017.04.15 (low)
(defsound test option-d
  (let [gate (impulse 1/4)
        env (rg-exp (lf-tri 1/8 -1) 1/4 1)
        freq (env-gen (envelope [0 180 180 160 160] [0 1 1 1]) gate)]
    (leak-dc (lpf (tanh (* 4 (+ (* (pm-osc freq (/ freq 4) 4)
                                   (rg-lin (lf-tri 12) 3/4 1)
                                   (rg-lin (lf-tri 7) 3/4 1))
                                (let [vib (sin-r 1/8 0.95 1)
                                      freq (* vib freq)]
                                  (distort (clip2 (pm-osc (/ freq 2) (/ freq 4) 8)
                                                  (rg-lin (lf-saw 0.3 -1) 0.8 1)))))
                           env))
                  freq))))

;; 2017.04.15
(defsound test option-d
  (+ (rlpf (latch:ar (saw (dt 10 [100 180 320])) (impulse 400))
           (line 100 20000 30)
           0.5)
     (+ (* (pm-osc 120 50 8)
           (env-gen (env-perc 0.05 1) (impulse (dt:kr 2 [[1/4 1/2 1/2 1 1]
                                                         [3/2 3/2 2 3 4]
                                                         [8 8 8 8 8 8 8]
                                                         [16 16 16 16 16]])
                                               1/2)))
        (* (step 7)
           (rhpf (white-noise) (env-gen (envelope [8000 8000 1] [20 10]) :action FREE))
           (lf-pulse 8)))))

;; 2017.04.16
(defsound test option-d
  (m-map (fn [gate-freq gate-phase timing]
           (let [gate (impulse gate-freq gate-phase)
                 freq (dq gate (take 10 (iterate #(* 1.2 %) 440)))
                 f-env (env-gen (envelope [0 0 100] [0 0.1]) gate)]
             (* (reduce (fn [snd _] (* 0.75 (+ (clip2 snd 0.9) (distort snd))))
                        (* (saw (* freq (sin-r (+ (/ freq 2) f-env)
                                               (env-gen (envelope [0 0.9 0.8 0.5 0] [0 0.3 0.5 0.2]) gate)
                                               1)))
                           (env-gen (env-perc 0.05 1) gate))
                        (range 1 4))
                (step timing))))
         [1 2 5 14 32]
         [0 1/2 3/4 0 0]
         [0 2 6 10 12]))

;; 2017.04.17
(defsound test option-d
  (let [base-freq (+ (rg-exp (switch (lf-pulse 1/4 0 1/4)
                                     (lf-noise0 8)
                                     (lf-noise2 4)) 10 2000))]
    (+ (m-map (fn [freq]
                (let [freq (+ base-freq freq)]
                  (resonz (* (rg-lin (lf-tri (sin-r 1.33 4 32)) 0.2 1) (saw (* 3/2 freq)))
                          (sin-r 0.4 freq (* 2 freq)) 0.3)))
              [1000 2400 3700])
       (* (saw 40) (gray-noise) (env-gen (env-perc 0.05 0.2) (impulse 1/2)))
       (let [gate (pattern (impulse 8) [0 0 1 1 1 1 1 1])
             freq (dq gate (reverse (n-range 4000 12000 6)))]
         (* (bpf (white-noise) freq 0.3)
            (env-gen (env-perc 1e-4 0.08) gate)))
       (* 1/4 (sin-osc (rg-lin (lf-pulse 4) 0 400))
          (lf-pulse 8 0 1/8)))))

;; 2017.04.18 (like)
(defsound test option
  (let [rythm 1]
    (splay
     (map (fn [i freqs]
            (m-map #(* 256 (bpf (white-noise) %2 0.1)
                       (env-gen (env-perc 0.01 %3)
                                (* (lf-pulse (/ 1 i)) (impulse rythm %))))
                   (n-range 0 1 (count freqs))
                   freqs
                   (cycle [0.5 0.18 0.1])))
          (range 1 9)
          [[3200 3200 2440 3700 2000 2200]
           [200 200 300 400 600 600]
           [9000 8000 8500]
           (n-range 1000 8000 8)
           (reverse (n-range 100 2000 16))
           [5000 6000]
           [180 180 180 180 5000 5000 5000 5000]
           [800 400 600 200 300 600]]))))

;; 2017.04.21
(defsound test option-d
  (let [param (env-gen (envelope [32 16 16 8 8 1 0] [0.3 3.7 4 4 8 8]) (impulse 1/32))
        env (lf-saw param)]
    (+ (m-map #(* (lf-pulse 1/8 %1) env (lf-tri %2))
              (n-range 0 1 8)
              (shuffle (take 8 (iterate #(* 2 %) 150))))
       (tanh (* (/ 1 param) (lpf (saw 28) 300))))))

;; 2017.04.21
(defsound test option-d
  (let [gate (switch (lf-pulse 1/2)
                     (impulse 6)
                     (pattern (impulse 12) [[1 1 0 0 1 1 0 0 1 1 0 0]
                                            [0 0 1 1 0 0 1 0 1 1 1 1 0 0]]))
        gate2 (impulse 8)
        gate3 (impulse 2 1/2)
        gate4 (impulse 1/2 1/2)
        f-env (env-gen (envelope [0 220 80] [0 0.05]) gate3)]
    (+ (* 2 (bpf (gray-noise) 1200 0.5) (env-gen (env-perc 0.0 0.05) gate))
       (* (bpf (white-noise) 1600) (env-gen (envelope [0 0 1 0] [0.05 0.01 0.02]) gate))
       (* (bpf (white-noise) 8000) (env-gen (env-perc 1e-8 0.02)
                                            (* (lf-pulse 1/2 3/4 3/4) gate2)))
       (* (lpf (* (lf-tri f-env) (brown-noise)) (dq gate3 [250 120 120 1]))
          (env-gen (env-perc 0.05 0.5) gate3))
       (* 1/2 (sin-osc (rg-lin (lf-noise2 8) 3000 8000))
          (env-gen (envelope [0 0 1 0] [0.3 0.1 0.3]) gate4))
       (* (sin-osc (rg-lin (lf-noise2 8) 3000 8000))
          (lf-pulse 8 0 1/4)
          (env-gen (envelope [0 0 1 0] [0.3 0.1 0.8]) gate4)))))

;; 2017.04.23
(defsound test option-d
  (let [gate (impulse 1)
        gate2 (pattern (impulse 8) [[0 0 1 0 0 0 1 0]
                                    [0 0 1 0 0 0 1 0]
                                    [0 0 1 0 0 0 1 0]
                                    [0 1 0 0 1 1 1 1]])
        gate3 (pattern (impulse 4) [[1 1 1 0]
                                    [1 1 1 0]
                                    [1 1 1 0]
                                    [1 1 1 1]])
        gate4 (impulse (dt:kr 4 [4 8]))]
    (+ (* (bpf (white-noise) 1800) (env-gen (env-perc 0.01 0.25) gate))
       (* (bpf (white-noise) 5000) (env-gen (env-perc 0 0.08) gate2))
       (* (bpf (white-noise) 400 0.3) (env-gen (env-perc 0.03 0.2) gate3))
       (let [freq (dq gate4 [500 300 800 400])
             rq-env (env-gen (envelope [0 1 0.01] [0 0.2]) gate4)]
         (* (rlpf (white-noise) freq rq-env) (env-gen (env-perc 0.01 0.2) gate4))))))

;; 2017.04.24
(defsound test option-d
  (let [rythm (sin-r:kr 1/4 1 8)
        gate (impulse rythm)
        freq (lin-exp (m-map #(lf-pulse % -1) [1/2 1/4 4]) -1 1 480 3400)
        freq2 (latch:ar freq gate)]
    (+ (* (clip2 (sin-osc freq2) (sin-r 2.3 1/4 1)) (env-gen (env-perc 0.01 0.3) gate))
       (switch (lf-pulse 1/4 1/4)
               (* 1/8 (sin-r 1/8 1/4 1) (pulse (* 1/2 3/2 freq)))
               (free-verb (m-map #(* (rlpf (brown-noise) (* %2 freq) 0.1) (lf-pulse 8 %))
                                 (n-range 0 1 12)
                                 (take 12 (iterate #(* 1.5 %) 1/8)))
                          (sin-r 0.8 0 1) 2)))))

;; 2017.04.25
(defsound test option
  (let [gate (impulse 2)
        freq (switch (lf-pulse 1/4)
                     (+ (dq gate [800 1100]) (dt 4 [0 200 1000 1800]))
                     (rg-exp (lf-saw (dt 2 [-1/4 -1/4 1/4]) -1) 3000 200))
        snd (* (rlpf (gray-noise) freq 0.1) (env-gen (env-perc 0.2 0.18) gate))]
    (tanh (+ (ringz snd (rg-exp (m-map lf-pulse [3 7 1/4 1/8]) 400 8000) 0.01)
             (* (sin-osc (/ freq 4)) (env-gen (env-perc 0 0.0001)
                                              (dust:kr (x-line:kr 10 300 16))))))))

;; 2017.04.26
(defsound test option
  (let [freq (rg-lin (lf-tri 1/3) 300 800)]
    (switch (lf-pulse 1/4)
            (* (lf-tri freq) (lf-pulse (dt:kr 4 [8 2 16 1])))
            (* (tanh (rlpf (* 20 (saw 15) (saw 120) (gray-noise)) freq 0.2))
               (env-gen (env-perc 0.01 0.4) (impulse (dt:kr 6 [2 4])))))))

;; 2017.04.27
(defsound test option
  (let [gate (impulse 1)
        f-env (env-gen (envelope [0 1200 1200 200] [0 0.02 0.03] EXP) gate)
        gate2 (* (lf-pulse 1 1/2) (impulse 4))]
    (tanh (+ (* (rhpf (white-noise) f-env 0.5)
                (env-gen (envelope [0 1 1 0] [0.002 0.05 0.01]) gate))
             (* (rhpf (white-noise) 600) (env-gen (env-perc 0 0.08) gate2))
             (* 1/4 (rlpf (white-noise) (+ (switch (lf-pulse 1/4 3/4)
                                                   (rg-lin (lf-pulse 8) 1800 6200)
                                                   (rg-exp (lf-saw 1/4) 200 5000))
                                           (dq gate [3000 4000 0 2000])) 0.025))
             (let [gate (impulse 1/4 3/4)
                   freq (env-gen (envelope [0 400 50] [0 0.25]) gate)]
               (* (sin-osc freq) (saw 180)
                  (env-gen (env-perc 0.2 2) gate)))))))

;;2017.04.28
(defsound test option
  (apply m-map #(let [gate %
                      freq %2]
                  (tanh (ringz (* gate (brown-noise) ) freq 0.2)))
         (transpose [[(impulse 4)     (rg-exp (m-map lf-pulse [4 7 1/3 1/8]) 300 1200)]
                     [(impulse 2 1/2) (rg-exp (m-map lf-saw [4 7 1/3 1/8]) 100 300)]
                     [(impulse 8 3/4) (rg-exp (m-map lf-saw [1/3 1/8 1/2]) 7000 10000)]
                     [(* (lf-pulse 1/4 1/2) (impulse 4 1/2))
                      (rg-exp (m-map lf-saw [1/3 1/8 1/2]) 800 9000)]])))

;; 2017.04.29
(defsound test option
  (+ (* (hpf (white-noise) 7000) (env-gen (env-perc 0.03 0.02) (impulse 4)))
     (let [gate (pattern (impulse 8)
                         [[1 0]
                          [0 0]
                          [0 1]
                          [0 1]])]
       (* 4 (bpf (white-noise) 400 0.2) (env-gen (env-perc 0.001 0.05) gate)))
     (* (brown-noise) (env-gen (env-perc 0.02 0.2) (impulse 1 1/2)))
     (* (bpf (white-noise) 2000 0.1) (env-gen (env-perc 0.08 0.03) (impulse 2 1/2)))
     ))

;; 2017.05.04
(defsound test option
  (switch (lf-saw 1/4)
          (switch (lf-pulse 4 0 (m-map lf-pulse [1/3 2.1 3/7 1/2]))
                  (* (sin-osc (* 440 (sin-osc (dt 2 [220 110])))))
                  (* 1/2 (saw (dt 2 [440 660])) (sin-osc 440)))
          (rlpf (free-verb (* (gray-noise) (env-gen (env-perc 0.01 0.1) (impulse 16)))
                           1 2)
                (rg-lin (lf-saw (rg-lin (lf-saw 1/4 -1) 1 8)) 100 240)
                (sin-r 3.3 0.1 0.5))))

;; 2017.05.05
(defsound test option
  (let [noise (lf-noise1 (x-line 1/2 32 16))]
    (+ (* 16 (sin-osc 220) (lpf (white-noise) (lin-lin noise -1 1 30 120)))
       (m-map #(let [gate (dust:kr 3)]
                 (* (sin-osc (lin-exp noise -1 1 % %2))
                    (env-gen (env-perc 0.01 0.08) gate)))
              [30 300 1800]
              [2800 600 4500]))))

;; 2016.05.06(server down)
(defsound test option
  (let [sound (saw (rg-lin (lf-pulse 2) 50 (dt 1/2 [80 90 120])))]
    (echo sound 5 0.01 0.04 0.3)))

;; 2016.05.07
(defsound test option
  (let [freq (dt 1 [[500 600 800]
                    [450 600 800]
                    [1200 1000 800]])
        ratio [[1 1 1] [3/2 3/2 2] [4/5 5/4 4/5]]]
    (+ (echo (* 1/4 (sin-r 2.4 0.8 1)
                (m-map #(pulse (* % freq) (sin-r 3.1 0.1 0.4))
                       ratio))
             7 0.01 0.04 1)
       (echo (m-map #(let [p (* (lf-pulse 1/4 (* 1/4 %) (sin-r:kr 1.3 0 1/4))
                                (lf-pulse (sin-r:kr 1/2 4 12)))
                           noise (cubed (white-noise))]
                       (* p (rlpf noise (* (dt 1 [100 150 48 83])
                                           (dq p [4 7])
                                           %) 0.01)))
                    (range 4))
             3 0.01 0.02 0.1))))

;; 2017.05.15
(defsound test option
  (echo (apply m-map #(let [gate (step %2 1.5)
                            am-freq (env-gen (envelope [0 100 100 200 1000] [0 2 2 8])
                                             gate)]
                        (* (pulse %) (sin-osc am-freq) gate))
               (transpose [[5000 0]
                           [8000 5]
                           [6700 6]
                           [2000 6.5]
                           [800 8.5]
                           [1200 9.5]]))
        5 0.1 0.2 1))

;; 2017.05.22
(defsound test option
  (echo (+ (m-map #(let [gate (impulse 1/4 %)
                         f-env (env-gen (envelope [0 2 1] [0 0.07]) gate)
                         env (env-gen (env-perc 0.05 0.5) gate)]
                     (* 8 (pink-noise) (sin-osc (* f-env %2)) env))
                  (n-range 0 1 8 )
                  [2440 3440 440 640 940 1240 8480 7480 6480 5480 5480])
           (* (lf-pulse 4 0 1/4) (sin-osc (dt 2 [200 250])))
           (* (saw 20) (sin-osc 1600) (env-gen (env-perc 0.05 1) (impulse 1/2 1/2))))
        4 0.01 0.1 0.3))

;; 2017.05.29
(defsound test option
  (let [n (lf-noise2 1)]
    (echo (+ (rlpf (* (saw 30) (white-noise))
                   (lin-exp n 0 1 400 10000) 0.1)
             (switch n
                     (* (lf-pulse 16) (white-noise))
                     (* 1/4 (lf-saw 4) (m-map #(pulse (* % (lin-exp n 0 1 400 2000)))
                                              [3/2 4/3 5/4]))))
          8 0.01 0.05 0.08)))

;; 2017.06.04
(defsound test option-d
  (echo (let [freq (dt 2 [440 660])
              gate (impulse (dt:kr 3 [4 6]))
              gate2 (pattern gate [1 1 0 0 1 0])
              gate3 (pattern gate [0 0 0 1 0 1])]
          (tanh (+ (* (sin-osc (rg-lin (m-map lf-pulse [1/4 1/3 1/5]) 20 300))
                      (switch (lf-saw 1/8) (sin-osc freq) (pulse freq))
                      (env-gen (env-perc 0.01 0.2) gate))
                   (* 1/2 (gray-noise) (env-gen (env-perc 0.05 0) gate2))
                   (* (sin-osc (* (dq gate3 [1/2 1/2 1/3 1]) freq))
                      (env-gen (env-perc 0.05 0.5) gate3)))))
        3 0.001 0.01 0.2))

;; 2017.06.04 (like)
(defsound test option-d
  (+ (m-map #(* (switch (lf-saw 1/8 %2) (sin-osc (* dr %1)) (pulse %1))
                (env-gen (env-perc 3 3) (impulse 1/6 %2)))
            (n-range 300 3000 10)
            (n-range 0 1 10))
     (* (lf-pulse 1/4 1/2 (line:kr 0.1 0.8 16))
        (line 0 1 16)
        (crackle (rg-lin (lf-saw 4) 1 2)))))

;; 2017.06.05
(defsound test option-d
  (+ (let [freq (rg-exp (lf-saw (dt 1 [-2 4 -2 8])) 300 1400)]
       (* 1/4 (echo (switch
                     (sin-osc (rg-exp (lf-tri 1/4) 0.01 16))
                     (lf-tri freq)
                     (pulse freq))
                    4 0.01 0.1 0.1)))
     (let [freq (dt 2 [[1200 1600]
                       [1200 1600]
                       [2400 1600]])
           gate (impulse 2)
           f-env (env-gen (envelope [0 8 1] [0.001 0.1]) gate)]
       (* (sin-osc (* freq (sin-osc (* 30 f-env))))
          (env-gen (env-perc 0.05 0.5) gate)))
     (* (lf-pulse 1/4 1/4 1/8) (lf-pulse 16 0 1/4) (gray-noise))))

;; 2017.06.06
(defsound test option-d
  (let [freq (rg-exp (lf-saw 4) 500 3000)]
    (free-verb (+ (switch (lf-pulse 1/3)
                          (* 1/2 (lf-tri (rg-exp (sin-osc (m-map lf-pulse [1/3 7.4 1/7])) 200 8000))
                             (sin-osc (* freq (rg-lin (sin-osc 100) 1/4 1))))
                          (+ (* 1/4 (sin-osc (* 3/2 freq)))
                             (tanh (* 1/16 (rhpf (white-noise) (* 6 freq) 0.2)))))
                  (let [gate (impulse 2)
                        f-env (switch (lf-pulse 1/2)
                                      (env-gen (envelope [0 4 1] [0.001 0.15]) gate)
                                      (env-gen (envelope [0 3 1 8] [0 0.01 0.15]) gate))
                        freq (dq gate [150 120])]
                    (tanh (* 2.5 (sin-osc (* f-env freq)) (env-gen (env-perc 0.01 0.28) gate)))))
               0.4)))

;; 2017.06.07
(defsound test option-d
  (let [freq (env-gen (envelope [880 880 920 920 600 0] [3 4 2 5 1]) :action FREE)]
    (free-verb (+ (lpf (m-map #(* %2 (sin-osc 3.2) (sin-r 7.8 0.5 1) (pulse (* freq %) (sin-r 2.3 0.1 0.5)))
                              [1 5/4 5/2]
                              [1 (sin-r 0.3 0.5 1) (sin-r 0.1 0.2 1)])
                       (* (sin-r 0.8 0.99 1.2) freq))
                  (* (sin-r 0.15 1/8 0.2) (hpf (white-noise) (+ (* (sin-r 1/16 4000 10000))
                                                                (rg-exp (lf-saw 1) 10 3000)))))
               0.6)))

;; 2017.06.07 (like)
(defsound test option-d
  (let [gate (impulse 1)
        freq (* dr (dq gate [100 130 200 350 800 1280]))
        freq2 100]
    (free-verb (* (m-map #(sin-osc (* % freq (sin-osc (* %2 freq2))))
                         (take 15 (iterate #(* (+ 1 (rand 1)) %) 1))
                         (take 15 (iterate #(* (+ 1 (rand 0.1)) %) 1)))
                  (env-gen (env-perc 0.05 1) gate))
               (sin-r 0.3 0 1) 1)))

;; 2017.06.08 (like) uneri
(defsound test option-d
  (+ (m-map (fn [freq] (* (sin-osc freq) (sin-r (sin-r 0.2 (* 1/4 freq) (* 1/3 freq)) 0.3 1)
                          (clip:ar (sin-r 0.18 0.1 1) 0.1 0.5)))
            (flatten [ (take 10 (iterate #(* % 1.13) 800))
                      (take 5 (iterate #(* % 1.5) 1800))
                      (take 4 (iterate #(* % 2.4) 700))]))
     (free-verb (lpf (* (sin-r 3.2 0.5 1) (sin-r 50 0.1 0.3) (saw (* 80 (sin-r 20 0.8 1))))
                     300)
                (sin-r 0.32 0.2 1) (sin-r 0.27 0 10))))

;; 2017.06.08 (like)
(defsound test option-d
  (let [big-gate (impulse 1/20)
        freq-max  (env-gen (envelope [10000 10000 1000 1000 10000] [2 5 3 6]) big-gate)
        ;; (rg-exp (m-map lf-pulse [0.7 3.2 1.3 8.3]) 1000 10000)
        freq-min (env-gen (envelope [100 100 800 2000 5000] [2 8 2 4]) big-gate)]
    (tanh (* 30 (mix (repeatedly 30 #(let [gate (impulse 16 (rand 1))]
                                       (* (pulse (* dr (latch:ar (rg-exp (lf-noise1 8) freq-min freq-max) gate)))
                                          (env-gen (env-perc 0.05 0.2) gate)))))))))
;; 2017.06.09
(defsound test option-d
  (m-map #(let [gate (impulse 1/4 %)
                freq %2
                am-freq (rg-lin (sin-osc 2.8) (* 1/2 freq) (* 1/4 freq))]
            (* 16 (lf-tri freq) (lf-tri am-freq) (env-gen (env-perc 0.01 1) gate)))
         (shuffle (n-range 0 1 20))
         (take 20 (iterate #(* 1.08 %) 200))))

;; 2017.06.09 (like) noise scream
(defsound test option-d
  (let [freqs (reduce (fn [acc x] (cons (* (first acc) x) acc))
                      [100] (take 8 (cycle [1.1 2.1 3.1])))
        gate (impulse 1)
        f-env (env-gen (envelope [0 2 1 1 0] [0 0.2 0.4 0.4]) gate)]
    (tanh (* 12 (m-map #(let [freq (* f-env % dr)
                              fm-freq (* 1/8 freq)
                              fm-freq2 (* 1/8 fm-freq)]
                          (sin-osc (* freq (sin-osc (* fm-freq (sin-osc fm-freq2))))))
                       freqs)))))

;; 2017.06.09 (like) noise scream
(defsound test option-d
  (let [freqs (reduce (fn [acc x] (cons (* (first acc) x) acc))
                      [300] (take 10 (cycle [1.08 1.21 2.1])))
        gate (impulse 1)
        f-env (env-gen (envelope [0 2 1 1 0] [0 0.1 0.4 0.5]) gate)]
    (tanh (* 12 (m-map #(let [freq (* f-env % dr)
                              fm-freq (* 2/3 freq)
                              fm-freq2 (* 1/8 fm-freq)]
                          (sin-osc (* freq (sin-osc (* fm-freq (sin-osc fm-freq2))))))
                       freqs)))))

;; 2017.06.10 (like) noise aua-
(defsound test option-d
  (let [gate (impulse 1)
        f-env (env-gen (envelope [0 2 1 1 2 1] [0 0.08 0.1 0.08 0.2]) gate)]
    (switch (cubed (rg-lin (lf-saw 1/6) 0 1))
            (lf-tri (* f-env 8000))
            (tanh (* 32 (m-map #(let [freq (* f-env %)
                                      am-freq (* %2 freq)
                                      am-freq2 (* 1/5 am-freq)]
                                  (* (sin-osc freq) (sin-osc (* am-freq (sin-osc am-freq2)))))
                               (reduce (fn [acc x] (cons (* (first acc) x) acc))
                                       [2800]
                                       (take 10 (cycle [1.5 1.1 1.1])))
                               (cycle [1/4 1/8 1/16])))))))

;; 2016.06.11
(defsound test option-d
  (m-map #(let [gate (impulse 1.5 %2)
                freq (* % (dq gate [400 400 250 250 750 375]))]
            (* (sin-osc (lin-lin (clip (sin-osc:kr 8.1) 0.25 1) 0 1 (* freq 1/8) (* freq 2/8)))
               (sin-osc (* dr freq))
               (env-gen (env-perc 0.1 1) gate)))
         [1 3.5 5.8]
         (n-range 0 1/2 3)))

;; 2016.06.11
(defsound test option-d
  (let [gate (impulse 1)
        freq 800
        ratio (clip (sin-osc:kr (dq gate [1.1 0.7 2.8 8 12.3]))
                    (dt:kr 2 [0 0.25 0.8]) 1)]
    (* (tanh (m-map (fn [freq] (* 20
                                  (sin-osc (lin-lin ratio
                                                    0 1
                                                    (* freq 1/8) (* freq 2/8)))
                                  (sin-osc freq)))
                    (reduce (fn [acc x] (cons (* (first acc) x) acc))
                            [200]
                            (take 20 (cycle [1.1 1.3 1.25])))))
       (env-gen (env-perc 0.05 1) (impulse 1)))))

;; 2017.06.12
(defsound test option-d
  (let [r-freq (dt 4 [0.01 0.2 7])]
    (m-map #(let [freq %
                  fm-freq (* freq 1/8 (sin-r r-freq 0.1 1))]
              (sin-osc (* freq (sin-osc fm-freq))))
           (reduce (fn [acc x] (cons (* (first acc) x) acc))
                   [400]
                   (take 12 (cycle [2.1 1.1 1.2]))))))


(defsound test option-d
  (let [freq 800
        snd (pulse freq)
        mixed (m-map lf-saw [(dt 6 [1 12 18])
                             (dt 6 [8 60 38])
                             (dt 6 [3 1/3 300])])
        sweep (lin-exp mixed -1 1 100 2000)]
    (moog-ff snd sweep 3.8)))

(defsound test option-d
  (splay (leak-dc (map #(ringz (* 4 %1 (white-noise)) (* dr %3) %2)
                       [(impulse 2)
                        (impulse (rg-exp (lf-saw:kr 1/8) 1/10 8) 0.1)
                        (impulse (rg-exp (lf-saw:kr 1/4) 1/10 8))
                        (impulse (rg-lin (lf-pulse:kr 1/6 0 7/8) 16 4))
                        (impulse 1/8)]
                       [0.5 0.1 0.2 0.01 1]
                       [300 560 1200 2400 600]))))

(defsound test option
  (let [trig (impulse 8)
        freq (dq trig (flatten [ (range 400 1200 100)
                                (range 2000 2200 50)]))
        env (env-gen (envelope [0,1,0.5,0.0] [0.02,0.1,0.1]) trig)
        filter (* 0.3 (b-low-pass-4 (pulse freq 0.01) freq 0.01))]
    (comb-l (pan2 (* env (switch (line 0 1 8) filter (distort filter))))
            1/8 1/8 1.2)))

(defsound test option-d
  (* 8 (rlpf (+ (lf-tri 50) (crackle 1.5)) 300 0.5)
     (env-gen (env-perc 0.005 0.1) (impulse 4))))



(defsound test option
  (let [sound (* (sin-osc (+ 400 (* 1200 (lf-noise0 4))))
                 (squared (abs (lf-tri:ar 0.5))))
        filtered (echo sound 5 0.01 0.04 1)]
    (* 2 (+ filtered sound))))

(defsound test option
  (let [sound (* (sin-osc (+ 400 (* 1200 (lf-noise0 4))))
                 (squared (abs (lf-tri:ar 0.5))))
        filtered (reduce (fn [acc x] (allpass-c acc 0.2 (+ 0.01 x) 1))
                         sound
                         (repeatedly 5 #(rand 0.04)))]
    (* 2 (+ filtered sound))))




;; 2017.06.18
(defsound test option-d
  (let [max (line 3.8 3.56 16)
        freq (rg-exp (logistic (rg-lin (lf-tri 1/2) 3.55 max)) 100 10000)]
    (switch (lf-pulse 1/4)
            (tanh (* 2 (lf-saw 1/4) (sin-osc freq)))
            (* (logistic (line 3 4 16) 8000 )
               (env-gen (env-perc 0.001 0.1) (impulse 8))))))

;; 2017.06.26 (like)
(defsound test option-d
  (m-map #(let [gate (dust:kr 30)
                freq (latch:ar (rg-exp (lf-noise1 3) 1200 4800) gate)]
            (* (step %) 8
               (sin-osc freq)
               (env-gen (env-perc 0 0.03) gate)))
         [0 3 6 8 8.6 9 10]))

;; 2017.06.26
(defsound test option-d
  (m-map (fn [t ratio] (let [gate (dust:kr 30)
                             freq (latch:ar (rg-exp (lf-noise1 3) 1200 4800) gate)]
                         (* (step t) 8
                            (m-map #(sin-osc (* % freq)) [1 ratio (* ratio ratio)])
                            (env-gen (env-perc 0 0.03) gate))))
         [0 3 6 8 8.6 9 10]
         (repeatedly #(* (rand) 3))))

;; 2017.06.27 (jump)
(defsound test option
  (splay (* 8
      (map #(* (env-gen (env-perc 0.02 0.33) (impulse 4 %))
                 (sin-osc (sin-r (dt 2 [30.8 3.2 100.8]) (* 100 %2) (* (dt 6 [120 180]) %2))))
             (n-range 0 1 8)
             (map #(* % %) (range 1 9)))
      (env-gen (env-perc 0.01 0.8) (impulse 1)))))

;; 2017.06.28 (don-ten)
(defsound test option
  (splay (map #(let [freq %2
                 freq2 (env-gen (envelope [0 10 100 300] [0 0.5 2.3]) (impulse 1/4 %))]
             (* 8 (sin-osc (* freq (sin-osc (* freq 1/2 (sin-osc freq2)))))
                (lf-pulse 1/8 %)))
          (n-range 0 1 20)
          (take 20 (iterate #(* 1.2 %) 300)))))

;; 2017.06.28
(defsound test option
  (splay (* (map #(let [freq %2
                    freq2 (env-gen (envelope [0 10 100 300] [0 0.5 2.3]) (impulse 1/4 %))]
                (* 8 (sin-osc (* freq (sin-osc (* freq 1/2 (sin-osc freq2)))))
                   (lf-pulse 1/4 %)))
             (n-range 0 1 20)
             (take 20 (iterate #(* 1.2 %) 300)))
      (env-gen (env-perc 0.03 0.3) (impulse 8)))))

;; 2017.06.29
(defsound test option-d
  (let [gate (impulse 2)
        f-env (env-gen (envelope [0 1 2 1] [0 0.001 0.05]) gate)
        env (env-gen (env-perc 0.05 0.5) gate)
        freq (* 180 f-env)
        am-freq  (sin-r 0.4  (* 0.3 freq) (* 0.35 freq))
        am-freq2 (sin-r 0.23 (* 0.25 freq) (* 0.4 freq))]
    (* (+ (* (sin-osc freq)
             (sin-osc am-freq)
             (sin-osc am-freq2)
             )
          (sin-osc freq))
       env
       (sin-r 0.22 0.5 1))))

;; 2017.06.29
(defsound test option
  (splay (map #(let [gate (impulse 2 %)
               max (dq gate [8 3/2 2 3/2])
               f-env (env-gen (envelope [0 1 max 1] [0 0.1 0.2]) gate)
               freq %2]
           (sin-osc (* f-env freq)))
        (n-range 1/4 1 8)
        (reduce (fn [acc x] (cons (* x (first acc)) acc))
                [150]
                (take 8 (cycle [1.7 1.24]))))))

;; 2017.06.29
(defsound test option-d
  (let [base-freq (line 30 60 16)
        freq (sin-r 1.3 base-freq (+ base-freq 10))]
    (tanh (distort (* 4 (lf-tri freq)
                      (sin-osc (* 0.6 freq))
                      (sin-osc (* 0.62 freq)))))))

;; 2017.06.29
(defsound test option
  (let [freq 120]
    (splay (map #(* (/ 1 %) (sin-osc (* % freq (sin-r 2.8 0.99 1)) (* (sin-r 3.3 0 1)
                                                                (sin-r 8.2 0.8 1)))
              (sin-r 2.7 0.7 1))
          (range 1 18 3)))))

;; 2017.06.29 (moku-gho)
(defsound test option-d
  (let [n 12
        gate (impulse 1)
        freq 180]
    (* (m-map #(* 6 % (sin-osc (* dr %2 freq)))
            (map #(/ 1 %) (range 1 n))
            (map #(Math/pow 0.987 (* % %)) (range 2 (inc 12))))
       (env-gen (env-perc 0.05 0.5) gate))))

;; 2017.06.29 (melodious)
(defsound test option
  (splay (map (fn [phase] (let [n 12
                          gate (impulse 1/4 (pow phase 1.4))
                          freq (latch:ar (midicps (round (rg-lin (lf-noise1 2) 60 80) 2)) gate)]
                      (* 10 (m-map #(* 6 % (sin-osc (* (sin-r 2.1 %2 1) freq)))
                                   (map #(/ 1 %) (range 1 n))
                                   (map #(Math/pow 0.983 %) (range 2 (inc 12))))
                         (env-gen (env-perc 0.01 0.8) gate))))
        (n-range 0 1 8))))

;; 2017.06.29
(defsound test option-d
  (let [freq (* dr (env-gen (envelope [1000 2880 2880 500 400] [1/4 3 4 3])))
        env (env-gen (envelope [0 1] [1/4]))
        n 12]
    (* env 8
       (m-map #(let [min-f %2
                     min-v %3]
                 (* (/ 1 %) (sin-r 2.8 min-v 1) (sin-osc (* (sin-r 3.3 min-f 1) (* % freq)))))
              (range 1 n)
              (map #(Math/pow 0.997 %) (range 1 n))
              (map #(Math/pow 0.9 %) (range 1 12))))))

;; 2017.06.29
(defsound test option-d
  (let [freq 110
        gate (impulse 1)
        f-env (env-gen (envelope [0 8 1] [0 0.01]) gate)
        env (env-gen (env-perc 0.02 1) gate)
        snd (m-map #(pulse (* % freq dr f-env)) [1 3/2 7/3])]
    (* env (+ (rlpf snd  (* 3 freq) 0.7)
              (* 1/4 (rlpf snd (* (sin-r 3.3 3 8) freq) 0.7))))))

;; 2017.06.29 (good)
(defsound test option-d
  (let [freq 50
        gate (impulse 1)
        r (sin-r 3.3 (dq gate [3.3 1 12]) 4)]
    (free-verb (tanh (* 8  (rlpf (m-map #(pulse (* % freq)) [1 3/2 7/3]) (* r freq) 0.7)))
               1)))

;; 2017.06.29
(defsound test option
  (let [freq 4000]
    (* (sin-osc (rg-lin (lf-noise1 5000) freq (* 1.05 freq)))
       (env-gen (env-adsr) (impulse 1)))))

;; 2017.06.29
(defsound test option
  (let [freq 4000
        width 1.02]
    (repeatedly 2 #(* (sin-osc (rg-lin (brown-noise) freq (* width freq)))
                      (env-gen (env-adsr) (impulse 1))))))

;; 2017.06.30
(defsound test option
  (let [n 30]
    (->> (map #(let [gate (dust:kr 1/4)
                     dur 4]
                 (* (pan2 (sin-osc %2)
                          (sin-r 0.7 -1 1)
                          ;; (env-gen (envelope (map (fn [x] (* % x)) [0 -1 1]) [0 2]) gate)
                          )
                    (env-gen (envelope [0 1 1 0] [0.08 0.5 2]) gate)))
              (repeatedly n #(choose [-1 1]))
              (take n (iterate #(* 1.2 %) 30)))
         (transpose)
         (map mix))))

;; 2017.07.01
(defsound test option-d
  (let [xs (flatten (map-indexed
                     (fn [i x] (if (= (mod i 2) 0) [x x x] [x]))
                     (range 440 880 20)))
        fm-freq (lin-lin (clip:ar (sin-osc 0.7) 0 1) 0 1 100 400)]
    (tanh (let [gate (impulse (rg-lin (lf-saw:kr 0.4 -1) 4 12))
                freq (dq gate xs)]
            (* 2 (pulse (* dr freq (sin-osc fm-freq))) (env-gen (env-perc 0.01 0.3) gate))))))

;; 2017.07.02
(defsound test option-d
  (let [freq (env-gen (envelope [0 800 800 2000 0] [0 3 0.5 0.25]) (impulse 1/4))]
    (splay (map #(* (sin-osc (* freq % (sin-r %2 %3 1)))
              (sin-r %4 %5 1))
          (range 1 8)
          (take 10 (iterate #(* 1.5 %) 10))
          (reverse (take 10 (iterate #(* 0.99 %) 1)))
          (take 10 (iterate #(* 1.1 %) 10))
          (take 10 (iterate #(* 0.92 %) 1))))))

;; 2017.07.02
(defsound test option-d
  (let [gate (impulse 1)
        ratio (env-gen (envelope [0 0.1 (x-line:kr 0.8 0.99 12)]
                                 [0 (line:kr 0.2 0.08 12)]) gate)
        env (env-gen (env-perc 0.05 1) gate)
        freq 3800]
    (* (lf-tri (rg-lin (gray-noise) (* ratio freq) freq))
       env)))

;; 2017.07.02
(defsound test option-d
  (let [gate (impulse 1)
        ratio (env-gen (envelope [0 0.1 (x-line:kr 0.8 0.99 12)]
                                 [0 (line:kr 0.2 0.08 12)]) gate)
        env (env-gen (env-perc 0.05 1) gate)
        freq 800]
    (* (lf-tri (rg-lin (gray-noise) (* ratio freq) freq))
       env)))

;; 2017.07.06
(defsound test option-d
  (let [gate (impulse 1/4)
        rate (env-gen (envelope [0 0 1 1] [0 2 1]) gate)]
    (* (sin-osc (* (lin-lin rate  0 1 100 8000)
                   (sin-r (lin-exp rate 0 1 100 400) 0.7 1)
                   (sin-r (lin-exp rate 0 1 80 0.3) 0.3 1)))
       (env-gen (envelope [1e-6 1 1 1e-6] [1.2 0.8 2] :exp) gate))))

;; 2017.07.16
(defsound test option-d
  (let [r (+ (line 0 10 10)
             (cubed (sin-osc 8.5)))
        freq (lin-lin r -1 11 800 4000)]
    (sin-osc freq)))

;; 2017.07.16
(defsound test option
  (let [r (+ (* 10 (lf-tri 1/8))
             (* 2 (cubed (sin-osc (dt:kr 2 [8 1 16 2/3])))))
        freq (lin-lin r -10 11 800 4000)]
    (switch (sin-r 1.3 0.2 1.0)
            (moog-ff (white-noise) freq 3.99)
            (sin-osc freq))))

;; 2017.07.17
(defsound test option
  (let [gate (impulse (dt:kr 1 [[2 2 4 1]
                                [2 4 2 1]]))
        freq (midicps (-> (rg-lin (m-map lf-pulse [1.2 1.8 0.4]) 60 90)
                          (latch:ar gate)
                          (round 1/2)))
        f-env (env-gen (envelope [0 5 1] [0 0.04]) gate)]
    (tanh (* 8 (free-verb (rlpf (saw (rg-lin (lf-pulse 16) (* 1/8 freq) (* 1/7 freq)))
                                (* f-env freq) 0.35)
                          0.5 0.6)
             (env-gen (env-perc 0.05 0.5) gate)))))


(defsound test option-d
  (let [gate (impulse 1)
        freq (dt 1 [440 580])]
    (free-verb (* (lpf (pulse freq (sin-r 2.8 0.5 1))
                       (* freq (sin-r (env-gen (envelope [0 0.4 8] [0 0.3]) gate) 2 4)))
                  (env-gen (envelope [0 1 1 0] [0.01 0.15 0.3]) gate))
               0.6 0.5)))


(defsound test option
  (let [gate (impulse 1)
        freq (lag-ud (dt 3 [440 580]) 0.8 1.2)]
    (lpf (map (fn [ratio] (m-map #(pulse (* ratio freq)
                                         (sin-r (* 5 %) (- 0.5 %) (+ 0.5 %))) (range 0 0.3 0.02)))
              [1.01 0.99])
         (* freq (sin-r (env-gen (envelope [0 8 0.4] [0 0.3]) gate) 2 8)))))

;; 2017.07.20 (like) uneri tensi
(defsound test option-d
  (let [freq 220
        f-env (env-gen (envelope [0 2.3 0.4] [0.18 1]) (impulse (dt:kr 4 [0.8 8 1/2 1/2])))]
    (free-verb (tanh (m-map (fn [ratio] (m-map #(* 8 (sin-osc (* ratio % freq))
                                                   (sin-r f-env 0.1 1)
                                                   (/ 1 %))
                                               (range 1 8 2)))
                            [1 8/3 5/3]))
               0.7 10)))


;; 2017.07.21
(defsound test option-d
  (let [freq-max (env-gen (envelope [220 225 240 220] [1 0.5 0.3]) (impulse 1/4))
        freq (sin-r 3.3 220 freq-max)
        f-env (env-gen (envelope [0 2.3 0.4] [0.18 1]) (impulse (dt:kr 4 [0.8 8 1/2 1/2])))]
    (free-verb (tanh (m-map (fn [ratio] (m-map #(* 8 (sin-osc (* ratio % freq))
                                                   (sin-r f-env 0.1 1)
                                                   (/ 1 %))
                                               (range 1 8 2)))
                            [1 8/3 5/3]))
               0.7 10)))

;; 2017.07.22 (like) simple
(defsound test option
  (splay (map (fn [i] (let [gate (impulse (/ (+ i 8) 16))
                            freq (dq gate (map midi->hz [72 74 72 76]))]
                        (tanh (* (sin-osc (* (* i 1/2) freq)) 4
                                 (env-gen (env-perc 0.05 0.5) gate)))))
              (range 1 7))))

;; 2017.07.23
(defsound test option
  (let [freq 440
        rm (env-gen (envelope [0.27 0.27 0.7] [8 8]))]
    (splay (map #(let [gate (impulse (/ (+ 4 %) 16))]
                   (* 8 (sin-osc (* freq % (sin-osc (* freq (sin-r 2.3 0.25 rm)))))
                      (env-gen (env-perc 0.05 2) gate)))
                (range 1 16 2)))))

;; 2017.07.24
(defsound test option
  (m-map #(let [gate (impulse 1/2 %)
                freq %2
                freq2 (env-gen (envelope [0 0 0.25 0.3 1] [0 1 0.5 0.5] EXP) gate)
                freq3 (env-gen (envelope [0 2 2 1.5] [0 1.3 0.8]) gate)]
            (* (sin-osc (* freq
                           (sin-osc (* freq freq2))))
               (env-gen (envelope [0 1 1 0] [0.8 0.5 0.7]) gate)))
         (n-range 3/4 1 4)
         [440 650 200 300]))

;; 2017.07.26 (like) rythm
(defsound test option-d
  (let [rythm (dt:kr 1 [1 1 2 1/4])]
    (+ (m-map #(let [gate (impulse rythm %1)
                     f-env (env-gen (envelope [0 7 1] [0.002 0.011]) gate)
                     freq (* f-env %2)]
                 (* (rlpf (white-noise)
                          (rg-lin (pink-noise) freq (* 1.5 dr freq))
                          0.05)
                    (env-gen (env-perc 0.05 0.5) gate)))
              [0.3 0.38 0.6 0.8]
              [440 300 750 1300])
       (m-map #(tanh (ringz (* (white-noise)
                               (env-gen (env-perc 0.001 0.01) (impulse 1/2 %3))) % %2))
              [200 980 120]
              [0.5 0.1 1]
              [(dt:kr 4 [0 3/4])
               (dt:kr 2 [1/2 1/4])
               7/8])
       (* (sin-osc (* dr (rg-lin (pink-noise) 4000 10000)))
          (env-gen (env-perc 0.0001 0.08) (* (impulse 12)
                                             (lf-pulse 1/2 1/2 1/4)))))))

;; 2017.07.27 (like) midi
(defsound test option-d
  (m-map #(let [gate (impulse %2 %)
                freq (latch:ar (midicps (round (rg-lin (sin-osc 1.3) 70 90) 1)) gate)]
            (* 4 (sin-osc (rg-lin (gray-noise) freq (* dr 1.08 freq)))
               (env-gen (env-perc 0.01 0.8) gate)))
         (n-range 0 1 16)
         (n-range 1/4 1.5 16)))

;; 2017.07.27 (like) water
(defsound test option
  (let [max-note (env-gen (envelope [0 90 120] [0 1.5] :step) (impulse 1/2))]
    (splay (map #(let [gate (impulse %2 %)
                 freq (latch:ar (midicps (round (rg-lin (sin-osc 1.3) 30 max-note) 1)) gate)]
             (* (sin-osc (rg-lin (gray-noise) freq (* 1.08 freq)))
                (env-gen (env-perc 0.01 0.8) gate)))
          (n-range 0 1 16)
          (n-range 1/4 1.5 16)))))

;; 2017.07.27
(defsound test option
  (+ (let [gate (impulse 1/2)
           freq 120
           f-env (env-gen (envelope [0 8 1] [0 0.001]) gate)]
       (+ (* (+ (sin-osc (* freq f-env (sin-osc 30)))
                (* 1/16 (saw (* 2/3 freq))))
             (env-gen (env-perc 0.006 0.12) gate))
          (* (pink-noise) 1/32
             (env-gen (envelope [0 1 0.01 1 0.01 0] [0.1 0.08 1.2 0.1]) gate))))
     (let [gate (impulse 1/2 1/4)
           env (env-gen (envelope [0 1 0.0] [0.2 0.001]) gate)
           freq 300
           f-env2 (env-gen (envelope [0 1 20] [0 0.3] EXP) gate)]
       (* (rlpf (* (saw 100) (sin-osc 4400)) (* f-env2 freq) 0.8) 1/8
          env))))

;; 2017.07.29
(defsound test option
  (let [freq (midicps (env-gen (envelope [67 67 62 62 30 30
                                          70 70 75 1] [3 1 2 1 3 1 0.5 4 0.5])))]
    (splay (map (fn [freq-ratio env]
                  (m-map #(* (sin-osc (* % freq freq-ratio))
                             (lf-noise1 0.8)
                             env (/ 1 %))
                         (range 1 10 2)))
                [1 3/2 7/3 9/2]
                (map #(rg-lin (lf-tri 0.3 %) 0.25 1) (n-range 0 2 4))))))

(defsound test option
  (let [freq (midicps (env-gen (envelope [67 67 62 62 30 30
                                          70 70 75 1] [3 1 2 1 3 1 0.5 4 0.5])))]
    (+ (splay (map (fn [freq-ratio env]
                     (m-map #(* (sin-osc (* % freq freq-ratio))
                                (lf-noise1 0.8)
                                env (/ 1 %))
                            (range 1 10 2)))
                   [1 3/2 7/3 9/2]
                   (map #(rg-lin (lf-tri 0.3 %) 0.25 1) (n-range 0 2 4)))))))

;; 2017.08.01
(defsound test option-d
  (let  [freq 660
         cutoff 2200
         gate (line:kr 1 0 1)
         snd (+ (* 0.65 (mix (repeatedly 5 #(let [tune (ranged-rand 0.98 1)]
                                              (-> (saw (* tune freq))
                                                  (delay-c 0.01 (ranged-rand 0.0001 0.01)))))))
                (* 0.85 (mix (repeatedly 5 #(let [tune (ranged-rand 0.99 1)]
                                              (saw (* tune (* 1/2 freq)))))))
                (* 0.12 (pink-noise)))
         env (env-gen  (adsr 0.001 0.7 0.2 0.1) gate)]
    (-> snd
        (clip2 0.45)
        (rlpf (+ (* dr freq) (env-gen (adsr 0.001) gate :level-scale cutoff)) 0.75)
        (free-verb (sin-r 0.3 0.3 1) :room 1.8))))

;; 2017.08.02
(defsound test option-d
  (let [freq 440]
    (mix (+ (* 2/3 (repeatedly 5 #(lf-tri (* (ranged-rand 0.99 1.01) freq))))
            (* 1/16 (repeatedly 5 #(saw (* (ranged-rand 0.99 1.01) (* 1/2 freq)))))))))



;; 2017.08.28
(defsound test option-d
  (+ (m-map #(let [freq (env-gen (envelope [200 1000 500 8000 200]
                                           [0.1 0.7 0.1 0.01]
                                           [:exp :sin :step :step])
                                 (impulse 1 %))]
               (sin-osc freq))
            (n-range 0 1/2 8))
     (* 8 (cubed (lf-tri 1/3))
        (map lf-pulse [1/3 3 8]) (cubed (pink-noise)))))

;; 2017.08.29
(defsound test option-d
  (let [a (rg-lin (lf-pulse 12) 500 1200)
        b (-> (lin-lin (m-map lf-tri [1/3 1/7 3.2 8.2])
                       0 1 500 4800)
              (latch:ar (impulse 24)))
        freq (switch (* 2 (clip2 (sin-osc 1/4) 0.5)) a b)]
    (+ (blip freq)
       (* (dt 4 [1/2 1]) (tanh (* 100 (lf-saw 1/2 -2)
                                  (lf-pulse (dt:kr 4 [12 4]) 0 1/4)
                                  (bpf (gray-noise) (* 1/4 (+ a b)) 0.1)))))))

;; 2017.08.30
(defsound test option-d
  (let [base-freq (dt 1 [0 2000])
        freq (+ (lin-exp (clip2 (m-map lf-tri [3.2 1/3 1.3]) 0.2)
                         -0.2 0.2 300 3000)
                base-freq)]
    (switch (lf-saw 1/2)
            (m-map #(logistic (sin-r %1 3.4 3.99) %2)
                   [38.3 1.2 0.9]
                   [300 1000 3800])
            (switch (lf-pulse 8 0 7/8)
                    (saw freq)
                    (sin-osc freq)))))

;; 2017.08.31
(defsound test option-d
  (let [freq (lin-exp (m-map lf-saw [1/7 1/9 1/3]) -1 1 400 1800)]
    (+ (tanh (* 8 (m-map #(let [gate (impulse %2 %)]
                            (* (sin-osc (latch:ar freq gate))
                               (env-gen (env-perc 0.01 1.5) gate)))
                         (concat (n-range 0 1/2 8)
                                 (n-range 3/4 1 8))
                         (concat (n-range 0.25 0.28 8)
                                 (n-range 0.26 0.3 8)))))
       (let [gate (impulse 1/4)
             rq (env-gen (envelope [0.5 0.01 0.3] [0.08 3]) gate)]
         (* (rlpf (brown-noise) freq rq)
            (lf-pulse (dt:kr 2 [8 32]))
            (env-gen (env-perc 3.5 0.08) gate))))))

;; 2017.09.01 (beatiful)
(defsound test option
  (+ (let [freq (env-gen (envelope [220 240 1200 400] [4 2 4]))]
       (tanh (m-map #(let [gate (impulse %2 (rand))
                           vol (dq gate [1 1/2 1/4 1/4])]
                       (* (/ 1 %) (sin-osc (* % (latch freq gate))) vol 8
                          (env-gen (env-perc 0.01 0.4) gate)))
                    (take 12 (iterate #(* 1.28 %) 1))
                    (n-range 1 4 12))))
     (* (sin-osc 1/8)
        (lf-pulse (dt:kr 2 [8 8 32]))
        (rlpf (saw 80)
              (rg-exp (sin-osc 1) 80 520) 0.8))))

;; 2017.09.01 (like) rythm
(defsound test option-d
  (let [gate (impulse 6)
        freq (* (dq (impulse 1/2) [440 660 500])
                (dq gate [4/3 1])
                dr)
        vol (dq gate [1 1/2 2/3 1/4])]
    (+ (* (sin-osc freq) vol
          (env-gen (env-perc 0.01 0.2) gate))
       (tanh (* (bpf (white-noise) (* 4 freq) 0.01) 48
                (env-gen (env-perc 0.001 (dt:kr 1 [0.06 0.14]))
                         (impulse 3 (dt:kr 2 [0 1/2]))))))))

;; 2017.09.03 noise
(defsound test option
  (let [freq2 (rg-exp (m-map lf-pulse [1/3 0.8 0.9]) 100 8000)
        freq3 (rg-exp (m-map lf-saw [1/3 0.4 8.7]) 10 3000)]
    (switch (gray-noise)
            (distort (leak-dc (sin-osc (* 440 (saw (* freq2 (sin-osc freq3)))))))
            (rlpf (lf-noise0 1000) freq2 0.3))))

;; 2017.09.04 radio
(defsound test option
  (-> (m-map #(let [freq (lin-lin (switch (lf-pulse (dt:kr 4 [2 1/2])) (sin-osc 3.2 %2) (lf-noise1 6))
                                  -1 1 (rg-lin (lf-pulse 1/4 0 1/8) 200 800) 2800)]
                (* (/ 1 %1) (sin-osc (* %1 freq (rg-lin (pink-noise) 0.8 1.2)))))
             (range 1 9)
             (n-range 1/2 1 8))
      (* 12)
      (ringz (* 1000 (rg-lin (pink-noise) 0.5 1.2)) 0.1)
      (distort)))

;; 2017.09.06
(defsound test option
  (let [freq 440
        rate (env-gen (envelope [1 0.8 0] [5 8]))]
    (-> (* (saw (* freq (rg-lin (lf-noise2 [8 1]) 0.95 1.05)
                   (eff rate (* (sin-osc [1.3 0.7]) (sin-osc [3.3 1.5])))))
           (sin-osc 4800)
           (eff rate (* (sin-osc 1.3) (sin-osc [3.3 8.7])))
           (env-gen (envelope [0 1 1 0] [1 3 12]) (impulse 1/16)))
        (ringz (* (eff rate (sin-osc 12.8)) 1000) 0.1))))

;; 2017.09.07
(defsound test option-d
  (splay (map #(let [env (env-gen (env-perc 0.05 0.5) (impulse 1 %))]
                 (* 4 (sin-osc (* %2 (eff env (sin-osc (dt 4 [3.7 0.2 12])))))
                    env))
              (n-range 0 1 5)
              (shuffle (take 5 (iterate #(* 1.1 %) 440))))))

;; 2017.09.07 beam
(defsound test option
  (splay (map #(let [freq %3
                     env (env-gen (env-perc 0.05 0.5) (impulse %2 %))]
                 (* (sin-osc (* freq (eff env (white-noise)))) env))
              (n-range 0 1 5)
              (n-range 1 1.5 5)
              (take 5 (iterate #(* 2.3 %) 400)))))

;; 2017.09.08
(defsound test option-d
  (m-map #(* (saw (* %2 (eff (pink-noise) (sin-r 3.3 0 0.02))))
             (eff (env-gen (env-perc 0.3 1) (impulse (dt:kr 2 [1 1 1/2 2]) %))
                  (sin-osc 1.3)))
         (n-range 0 1 8)
         (take 8 (iterate #(* 1.1 %) 5000))))

;; 2017.09.09
(defsound test option-d
  (* (sin-r 1.3 0.2 1) (sin-r 9.1 0.75 1)
     (sin-osc (* 440 (eff (switch (lf-pulse 2) (pink-noise) (lf-noise0 8))
                          (sin-osc (rg-exp (m-map lf-pulse [1.3 0.7 1.8]) 0.5 8)))))))

;; 2017.09.10
(defsound test option-d
  (let [base-freq (dt:kr 4 [2800 1800 800 100])]
    (m-map #(let [gate (dust:kr 10)
                  env (env-gen (env-perc 0.001 0.02) gate)
                  freq (* base-freq % (latch:ar (rg-lin (white-noise) 0.95 1.05) gate))]
              (* (sin-osc freq) (- 1 env)))
           (take 30 (iterate #(* 1.08 %) 1)))))

;; 2017.09.10
(defsound test option-d
  (let [base-freq (dt:kr 4 [2800 1800 800 100])]
    (m-map #(let [gate (dust:kr 5)
                  env (env-gen (env-perc 0.001 0.1) gate)
                  freq (* base-freq % (latch:ar (rg-lin (white-noise) 0.95 1.05) gate))]
              (* (sin-osc freq) (- 1 env)))
           (take 30 (iterate #(* 1.15 %) 1)))))

;; 2017.09.11(like) frag
(defsound test option
  (let [freq-min [(dt:kr 1 [880 1100 880 880 1100 1155 1800 2800])
                  (dt:kr 2 [500 350 200 100])]
        freq (rg-lin (lf-pulse 10) freq-min (* freq-min 3/2))
        gate (impulse 1 [7/8 1])
        f-env (env-gen (envelope [0 1 1 0] [0.1 0.1 0.1]) gate)]
    (* (sin-osc (* freq (eff f-env (sin-osc 5.3))))
       (env-gen (env-perc 0.05 0.3) gate))))

;; 2017.09.11
(defsound test option
  (m-map #(let [max-freq (env-gen (envelope [450 450 800 2300] [4 2 2]) (impulse 1/8))
                freq (rg-lin (sin-osc 0.7 %) 440 max-freq)]
            (switch (lf-noise0 4)
                    (sin-osc (* freq (sin-r 18.9 0.8 1)))
                    (sin-osc (* freq (sin-r 8.9 0.5 3)))))
         (n-range 0 1/2 5)))


;; 2017.09.15 (like) vibrato
(defsound test option
  (splay (map #(let [gate (impulse 1/5 %2)
               freq (latch (* (dt:kr 4 (n-range 440 880 4)) %) gate)]
           (* (sin-osc (* freq ))
              (eff (env-gen (envelope [0 0 0 1] [0 2 2]) gate) (sin-r 3.3 0 0.5))
              (env-gen (envelope [0 1 1 0] [1 3 1]) gate)))
        (range 1 8)
        (reverse (n-range 1/4 1 8)))))

;; 2017.09.17
(defsound test option-d
  (let [freq (rg-lin (m-map lf-pulse [4.5 6.9 3.9 12.8]) 200 3000)]
    (m-map #(let [gate (impulse 1/4 %)
                  freq (latch freq gate)
                  env (env-gen (env-perc 0.18 0.3) gate)]
              (* (switch env
                         (saw (* dr freq (sin-r (* 1/8 freq) 0.7 1)))
                         (bpf (white-noise) freq))
                 env))
           (n-range 0 1 16))))

;; 2017.09.18 (like) water
(defsound test option
  (splay (map #(let [gate (* (impulse 16) (env-gen (envelope [0 1 1 0] [0 1/16 0]) (dust:kr %)))
               freq (latch:kr (lin-lin (mix [(sin-osc:kr 0.1)
                                             (lf-pulse:kr 8)]) -1 1 %3 %4) gate)]
           (* (sin-osc freq)
              (env-gen (env-perc 0.01 %2) gate)))
        [3 1 2 0.5]
        [0.15 0.8 0.08 1]
        [400 1800 8000 100]
        [1200 2000 12000 300])))

(defsound test option
  (splay (map #(let [gate (* (impulse 16) (env-gen (envelope [0 1 1 0] [0 1/16 0]) (dust:kr %)))
                     freq (midicps (latch:kr (lin-lin (mix [(sin-osc:kr 0.1)
                                                    (lf-pulse:kr 8)]) -1 1 %3 %4) gate))]
                 (* (sin-osc freq)
                    (env-gen (env-perc 0.01 %2) gate)))
              [3 1 2 0.5]
              [0.15 0.8 0.08 1]
              [42 76 82 92]
              [60 88 92 98])))

;; 2017.09.19
(defsound test option-d
  (let [freq (rg-lin (smooth-step (dt:kr 4 [1/8 1])
                                  (dt:kr 4 [1/16 2])) 440 880)]
    (sin-osc (* dr freq))))

;; suzu
(defsound test option
  (let [snd (* (saw (rg-lin (pink-noise) 3800 4200))
               (env-gen (env-perc 0.05 0.5) (impulse 1/4)))
        snd2 (comb-c snd 1/5 1/5 2)
        snd3 (comb-c snd2 1/3 1/3 3)]
    [(+ snd (* 1/3 snd3))
     (+ snd (* 2/3 snd2))]))

;; 2017.10.18(like)
(defsound test option
  (splay (* (lf-pulse [12 7.5 3])
               (ringz (* (lf-pulse 8) (sin-osc (sin-r 0.3 300 3000)))
                      (sin-r 1.7 1000 3000) (repeatedly 3 #(lf-noise1 0.5))))))

;; 2017.10.22
(defsound test option
  (let [n 12]
    (splay (map #(let [gate (impulse 1/20 %2)
                       f-env (env-gen (envelope [0 1 1 3] [0 0.1 1/4] :sine) gate)]
                   (* (eff (env-gen (envelope [0 0 1] [0.5 9]) gate)
                           (sin-r 3.3 0.7 1))
                      (sin-osc (* 880 % f-env
                                  (eff (env-gen (envelope [0 0 1] [0.5 9]) gate)
                                       (sin-r 3.3 0.98 1))))
                      (env-gen (envelope [0 1 1 0] [1 0.5 4]) gate)))
                (take n (iterate #(* 1.2 %) 0.5))
                (n-range 1/2 1 n)))))

;; 2017.10.23 (like) space
(defsound test option-d
  (m-map #(let [freq (sin-r 0.3 100 102)
                   gate (impulse 1/10 %2)]
               (* (sin-osc (* dr % freq (env-gen (envelope [0 1 3 3 1.5] [0 2 2 1/4]) gate)))
                  (env-gen (env-perc 2 10) gate)))
            (concat (take 8 (iterate #(* 3/2 %) 1))
                    (take 8 (iterate #(* 5/3 %) 1)))
            (concat (n-range 1/4 1/2 8)
                    (n-range 3/4 1 8))))

;; 2017.10.24
(defsound test option
  (splay (map #(let [freq %
                     gate (impulse 1/10 %2)]
                 (* (eff (env-gen (env-perc 5 0) gate) (sin-r 2.8 0.5 1))
                    (sin-osc (* freq (eff (env-gen (envelope [0 0 1] [2 5]) gate)
                                          (sin-r 3.3 0.85 1))))
                    (env-gen (env-perc 0.05 10) gate)))
              (shuffle (take 12 (reductions * 280 (cycle [8/7 12/8 7/4]))))
              (n-range 0 1 12))))

;; 2017.10.25 (like) flow
(defsound test option-d
  (mm-map #(let [pos (rg-lin (lf-tri %2 %3) -2 2)]
                (pan-lin pos (sin-osc (* 440 % dr))))
             (range 1 8)
             (map #(/ 1 %) (range 3 11))
             (shuffle (n-range 0 2 8))))

;; 2017.10.26 flow
(defsound test option-d
  (mm-map #(let [gate (impulse 1/8 %)
                    freq (* dr 640 %2)
                    pos (env-gen:ar (envelope [0 1 1 -1 -1 1] [0 1/2 3/2 1/2 3/2]) gate)
                    vol (env-gen:ar (envelope [0 1 0 1/6 0] [1 1 1 1]) gate)]
                (pan-lin pos (* vol (sin-osc freq))))
             (take 16 (reductions #(mod (* % %2) 1) 1 (cycle [1/3 7/4])))
             (reductions * 1 (cycle [11/9 13/11 22/36]))))

(l4/control :test :vol {:dur 16 :to 1})

;; 2017.10.27
(defsound test option
  (pan-exp (env-gen:ar (envelope [-2 -1 -1 1 1] [3 3 3 3]) :action FREE) (sin-osc 440)))
(defsound test option
  (pan-gau (env-gen:ar (envelope [-2 -1 -1 1 1] [3 3 3 3]) :action FREE) (sin-osc 440)))

(defsound test option                   ;flow
  (mm-map #(let [gate (impulse 1/8 %)
                 freq (* 640 %2)
                 pos (env-gen:ar (envelope [0 1 1 -1 -1 1] [0 1/2 3/2 1/2 3/2]) gate)
                 vol (env-gen:ar (envelope [0 1 0 1/6 0] [1 1 1 1]) gate)]
             (pan-exp pos (* vol (sin-osc freq))))
          (take 16 (reductions #(mod (* % %2) 1) 1 (cycle [1/3 7/4])))
          (reductions * 1 (cycle [11/9 13/11 22/36]))))

(defsound test option
  (mm-map #(let [gate (impulse 1/8 %)
                    freq (* 640 %2)
                    pos (env-gen:ar (envelope [0 1 1 -1 -1 1] [0 1/2 3/2 1/2 3/2]) gate)
                    vol (env-gen:ar (envelope [0 1 0 1/6 0] [1 1 1 1]) gate)]
                (pan-gau pos (* vol (sin-osc freq))))
             (take 16 (reductions #(mod (* % %2) 1) 1 (cycle [1/3 7/4])))
             (reductions * 1 (cycle [11/9 13/11 22/36]))))

;; flow
(defsound test option
  (mm-map #(let [pos (rg-lin (lf-tri %2 %3) -2 2)]
             (pan-gau pos (sin-osc (* :-freq %))))
          (range 1 8)
          (map #(/ 1 %) (range 3 11))
          (shuffle (n-range 0 2 8))))

;; 2017.10.28 flow
(defsound test2 option
  (mm-map #(let [freq (latch:ar (sin-r 0.3 440 2200) (impulse %))]
                (pan-gau (sin-r % -2 2) (sin-osc (* freq %2))))
             [0.05 0.07 0.19 0.09]
             [1 3/2 5/3 7/4 9/5]))

;; 2017.10.29 go!
(defsound test option-d
  (+ (apply m-map #(let [freq (env-gen (envelope [% % %2 %2 10000] [8 6 4 2]))]
                        (-> (* (sin-osc freq)
                               (blip (* freq dr (sin-r 22 0.9 1.1) ))
                               (lf-pulse (dt:kr 2 [3 3 6 6 8 8 12 30 60]) 0 7/8))
                            (* 8)
                            (distort)))
               (transpose [[440 1200]
                           [580 2400]
                           [780 4800]]))
        (let [gate (impulse (dt:kr 4 [1 1/2 1/4]))]
          (* 1/2 (lpf (saw (env-gen (envelope [0 330 80 120] [0 0.08 0.1]) gate))
                      (* dr 500))
             (env-gen (env-perc 0.05 (dt:kr 4 [0.5 1 2])) gate)))
        (let [gate (impulse (dt:kr 6 [8 12 16]))]
          (* (rlpf (pink-noise) (env-gen (envelope [0 10000 100] [0.01]) gate) 0.2) (env-gen (env-perc 1e-4 0.03) gate)))))

;; 2017.10.29
(defsound test option
  (mm-map (fn [freq] (pan-gau (sin-r 0.04 -2 2) (* (sin-osc 3.3) (sin-osc freq))))
             (take 8 (reductions * 220 (cycle [4/3 4/3 2])))))

;; 2017.10.29
(defsound test option
  (mm-map #(let [pos (sin-r 0.04 -2 2)
                    env (eff (lin-lin pos 0 2 0 1)
                             (sin-osc 10))]
                (pan-gau pos (* env (sin-osc %))))
             (take 8 (reductions * 220 (cycle [4/3 4/3 2])))))

;; 2017.10.30
(defsound test option
  (mm-map #(let [pos (sin-r (* 0.04 %) -2 2)
                    freq (* 220 %)
                    gate (impulse (* 4 %))
                    snd (* (rlpf (white-noise) freq 0.1)
                           (env-gen (env-perc 0 0.04) gate))]
                (pan-gau pos snd))
             (take 9 (reductions * 1 (cycle [5/4 6/5 4/3])))))


;; 2017.10.30
(defcgen pan-env-l
  [in {:default 0}
   attack {:default 0.05}
   release {:default 0.5}
   gate {:default 1}
   action {:default FREE}]
  (:ar (pan-gau (env-gen:ar (envelope [-1 0 1] [attack release]) gate FREE)
                in)))
(defcgen pan-env-r
  [in {:default 0}
   attack {:default 0.05}
   release {:default 0.5}
   gate {:default 1}
   action {:default FREE}]
  (:ar (pan-gau (env-gen:ar (envelope [1 0 -1] [attack release]) gate FREE)
                in)))

(defsound test option
  (let [p-freq (env-gen (envelope [1 1 10 1 0.1] [3 3 2 8]))
           release (env-gen (envelope [0.5 0.5 0.1 1 2] [3 3 2 8]))]
       (+ (mm-map #(pan-env-l (sin-osc (* % 440))
                              :release release
                              :gate (impulse p-freq (mod % 1)))
                  (take 7 (reductions * 1 (cycle [5/4 4/3]))))
          (mm-map #(pan-env-r (sin-osc (* % 220))
                              :release release
                              :gate (impulse p-freq (mod % 1)))
                  (take 7 (reductions * 1 (cycle [7/3 6/5])))))))

;; 2017.10.31
(defsound test option
  (splay (map (fn [freq phase]
                   (let [gate (impulse 1/12 phase)
                         r-env (map #(env-gen (envelope [0 4 1 1 8] [0 0.3 % 4]) gate)
                                    [0 0.5 0.3])
                         e-env (env-gen (envelope [0 0 0 1] [0 1 2]) gate)]
                     (mix (* 16 (eff e-env (sin-r 1.8 0.5 1))
                             (lpf (saw (* (eff e-env (sin-r 0.3 0.95 1.05)) freq [1 3/2 7/4]))
                                  (* freq r-env))
                             (env-gen (envelope [0 1 1 0] [0.12 2 1.9]) gate)))))
                 (shuffle (take 12 (reductions * 220 (cycle [4/3 7/6]))))
                 (n-range 0 1 12))))

;; 2017.11.01
(defsound test option-d
  (+ (* (pink-noise) (env-gen (env-perc 0 0.02) (impulse (dt:kr 1/2 [8 4]))))
     (* (brown-noise) (env-gen (env-perc 0.1 0) (impulse 1 (dt:kr 2 [3/7 5/7]))))
     (* (rlpf (gray-noise) (* dr (sin-r 10 100 5000)) (dt:kr 4 [0.8 0.2]))
        (env-gen (envelope [0 1 1 0] [0.01 0.4 0.01]) (impulse 1/2 1/8)))
     (let [gate (impulse 1/4 1/2)
           freq-max (dt:kr 4 [3000 500 1200])]
       (* (ringz (lf-noise0 100)
                 (env-gen (envelope [0 100 freq-max 100] [0 0.5 0.3]) gate) 0.01)
          (env-gen (env-perc 0.05 0.8) gate)))))

;; 2017.11.03
(defsound test option
  (let [gate (impulse (dt:kr 2 [1/2 1]))
           freq (sin-r 0.08 503 600)]
       (splay (map #(let [p-env (sin-r 0.2 1 4)
                          env (env-gen (env-perc (rand 0.1) 1) gate)
                          freq (* freq %)]
                      (* (lpf (switch (sin-r 1.3 0 1)
                                      (saw freq)
                                      (lf-tri freq))
                              (* freq p-env))
                         env))
                   (take 8 (iterate #(* 11/8 %) 1))))))

;; 2017.11.08 (like) se
(defsound test option-d
  (let [freq (rg-exp (sin-osc 3.3) 100 (* dr 2000))]
       (* (sin-osc (* 8 freq))
          (sin-osc (* (saw (* (sin-osc (* 2/3 freq)) 3/2 freq)) freq))
          (env-gen (env-perc 0.05 0.5) :action FREE))))

;; 2017.11.10
(defsound test option-d
  (-> (white-noise)
         (rlpf (* 220 dr (rg-exp (pink-noise) 1 3)) 0.01)
         (* 24)
         (tanh)
         (* (env-gen (env-perc 0.05 5) :action FREE))))

;; 2017.11.11 (noise)
(defsound test option-d
  (tanh (* 100 (reduce (fn [acc _] (* (brown-noise) acc))
                          (switch (lf-pulse 2) (sin-osc (rg-lin (lf-pulse 8) 100 300))
                                  (saw (* dr (rg-exp (sin-osc 0.24) 100 1000))))
                          (range 1 3)))))



;; 2017.12.02
(defsound test option-d
  (let [freq (-> (rg-lin (m-map #(lf-pulse % 0 1/8)
                                   [2.5 1/3 0.15])
                            60 90)
                    (midicps))]
       (-> (switch (lf-tri 0.3)
                   (* (sin-osc (sin-r 0.12 3000 4000)) (pink-noise))
                   (* (saw (* dr 80)) (gray-noise)))
           (bpf  freq (sin-r 2.3 0.1 0.4))
           (* 8)
           (tanh))))


;; 2017.12.25 (like) mysterious
(defsound test option-d
  (let [snd (m-map #(let [freq (* dr % (rg-lin (lf-noise0 8) 0.95 1.05))
                          freq2 (* % (rg-lin (lf-noise0 4) 0.99 1.01))]
                      (tanh (* 36 (bpf (bpf (white-noise) freq 0.1)
                                       freq2 0.01))))
                   (take 16 (reductions * (dt 2 [1000 2000 800 500])
                                        (cycle [5/4 7/6 11/10 4/5 6/7]))))]
    (switch (lf-tri 1/4) snd
            (* snd (env-gen (env-perc 0.05 0.1) (impulse 2))))))


;; 2018.02.01(low)
(defsound test option
  (detunr
      0.01
      (fn [dr]
        (+ (* (sin-r (rg-lin (lf-noise0 0.8) 0.5 3) 0.2 2)
              (m-map #(* %2 (sin-osc %) (sin-r 1.8 (sin-r 1.2 0.6 0.8) 1))
                     (take 11 (iterate #(* dr % 1.3) 15))
                     (map #(/ 5 %) (range 1 12))))
           (* 3 (lpf (lf-tri (* dr (sin-r 10 0.5 1) 50)) (sin-r 3.3 (line 120 50 10) 150))
              (env-gen (env-perc 0.05 6) (impulse 1/4)))))))



;; 2018.02.14
(defsound test option
  (detunr 0.01 (let [freq (rg-exp (m-map lf-pulse [0.15 0.2 0.8 2.8])
                                     1000 8000)]
                    (fn [dr]
                      (+ (m-map #(bpf (white-noise) (* dr % freq) 0.1)
                                (take 8 (iterate #(* 1.4 %) 1)))
                         (* (sin-osc 0.32) (sin-osc 0.01)
                            (switch (lf-tri 1/4)
                                    (clip:ar (* 8 (* (bpf (gray-noise) freq 0.07) (saw 50)))
                                             0.2)
                                    (* 1/8 (sin-osc (rg-lin (sin-osc 0.03) 20 80))))))))))

(def formants {:i [400 2000 2550]
               :e [530 1850 2500]
               :u [620 1200 2400]
               :oo [300 870 2250]
               :ee [270 2300 3000]
               :a [660 1700 2400]})
;; 2018.02.15 (like)
(defsound test option-d
  (let [gate (impulse 1/2)]
    (leak-dc
     (+ (tanh (free-verb (m-map #(* %1
                                    (ringz (* (lf-saw 32) (pink-noise))
                                           %2
                                           (sin-r 0.27 0.01 0.18)))
                                [1 2/3 1/8]
                                (map (fn [x] (env-gen (envelope x  [0.3 0.02 0.8 0.08 0.3]) gate))
                                     (transpose (map #(% formants) [:e :e    :i  :i :a :u]))))
                         1 10))
        (* (lf-noise0 4) (lf-pulse (dt:kr 2 [16 32])) (saw (* dr 8000)))))))

;; 2018.02.16
(defsound test option
  (+ (splay
      (map #(let [snd (* 1/2 (gray-noise) (saw %) (env-gen (env-perc 1e-3 0.01) (impulse %2)))]
              (tanh (leak-dc
                     (switch (sin-r 0.3 0 1)
                             snd
                             (comb-c snd 0.01 (sin-r 10.4 0.001 0.01) (rg-exp (lf-noise0 16)
                                                                              0.2 2))))))
           (n-range 20 1000 8)
           (n-range 1 4 8)))
     (splay (map #(* 4 (line 0 1 %) (sin-osc %2))
                 (shuffle (n-range 3 12 8))
                 (n-range 15 100 8)))))

;; 2018.02.17
(defsound test option-d
  (let [gate (m-map impulse (n-range 0.5 0.8 7))
           snd (* (lf-noise0 1) (env-gen (env-perc 0 0.00001) gate))
           sw (dq gate [0 0.2 0.65 0.8 0.1])]
       (+ (tanh (reduce (fn [acc [delay decay]]
                          (switch sw snd (comb-l acc delay delay decay)))
                        snd
                        (transpose [(n-range 0 0.1 12)
                                    (n-range 0.5 1 12)])))
          (saw (sin-r 0.25 28 35)))))

;; 2018.02.17 beeeeee
(defsound test option-d
  (let [gate (m-map impulse (n-range 0.5 0.8 7))
        snd (* (sin-osc (* dr 4000)) (env-gen (env-perc 0 0.00001) gate))
        sw (dq gate [0 0.2 0.65 0.8 0.1])]
    (tanh (reduce (fn [acc [delay decay]]
                    (switch sw snd (comb-l acc delay delay decay)))
                  snd
                  (transpose [(n-range 0 0.1 12)
                              (n-range 0.5 1 12)])))))

;; 2018.02.17
(defsound test option-d
  (reduce (fn [acc [x m]] (+
                              (* m acc)
                              (* (- 1 (abs m)) (delay-c acc x x))))
             (blip (x-line 0.1 80 12))
             (transpose [(n-range 0 1 6)
                         (cycle [0.1 -0.3 0.5 -0.5])])))

;; 2018.02.17 (like) machine
(defsound test option-d
  (let [freq (* dr (env-gen (envelope [440 440 880 880 100] [4 0.5 1 8])))]
    (mix (reductions (fn [acc x] (delay-l acc x x))
                        (* (lf-pulse 128 0 1/64) (sin-osc freq))
                        (take 8 (iterate #(* 1.8 %) 0.01))))))

;; 2018.02.18 (vosim)
(defsound test option-d
  (let [N 7
           T (rg-exp (m-map lf-pulse [0.3 0.54 0.8]) 0.00001 0.0025)
           M 0.016
           gate (impulse (/ 1 (+ (* N T) M)))
           env (env-gen (envelope [0 1 1e-4] [0 (* N T)] :exp) gate)
           snd (squared (sin-osc (/ dr T)))]
       (-> (* snd env)
           (free-verb 0.8 (x-line 0.1 10)))))

;; 2018.02.18
(defsound test option-d
  (reduce (fn [acc [a delay]] (+ acc (* a (delay-c acc delay delay))))
          (* (sin-osc (sin-r 44 100 (* dr 1000)))
             (env-gen (env-perc 0 1e-2) (impulse 1/2)))
          (transpose [[0.5 0.1]
                      [0.1 0.3]
                      [0.8 0.5]])))

;; 2018.02.19
(defsound test option
  (let [gate (impulse 1)]
       (splay (ringz (reductions (fn [acc x] (delay-n acc x x))
                                 (* (saw (sin-r 880 110 8800))
                                    (env-gen (env-perc 0.001 0.04) gate))
                                 (shuffle (n-range 0 1/4 12)))
                     (rg-exp (white-noise)
                             (dt 4 [2000 1000 100])
                             (dt 2 [10000 10000 5000 10000 1000 10000])) 0.4))))

;; 2018.02.20
(defsound test option
  (let [snd (blip (rg-exp (m-map lf-pulse [0.3 0.7 0.9 1.6]) 30 6000) 10000)]
       (splay (reductions (fn [acc x] (tanh (* 8 (delay-c acc x x))))
                          snd
                          (take 8 (iterate #(* 1.1 %) 0.1))))))

;; 2018.02.21
(defsound test option
  (let [snd (blip (rg-exp (m-map lf-pulse [0.3 0.7 0.9 1.6]) 30 6000) 10000)
           snd (+ (tanh (reduce (fn [acc x] (distort (* 8 (delay-c acc x x))))
                                snd
                                (take 8 (iterate #(* 1.1 %) 0.1))))
                  (* 1/8 snd))]
       [(* snd
           (env-gen (env-perc 0.02 1) (impulse 1)))
        (* snd
           (env-gen (env-perc 0.02 0.3) (impulse 2 1/2)))]))

;; 2018.02.22
(defsound test option
  (let [another (dt 3 [1 5 1/3 10])]
       (reduce
        (fn [acc x]
          (distort (* 8 (rlpf acc (* x (rg-exp (lf-pulse x) 200 800)
                                     another)
                              0.3))))
        (white-noise)
        (n-range 0.5 7 8))))

;; 2018.02.23  (like) simple but ...
(defsound test option
  (m-map #(let [freq (rg-exp (lf-saw (dt:kr 4 [-1/4 25 80]) %2) 100 %)]
               (rlpf (white-noise) freq 0.01))
            (n-range 1000 8000 8)
            (n-range -1 0 8)))


;; 2018.02.24
(defsound test option-d
  (m-map #(let [gate (impulse 1/6 %2)
                   f-env (env-gen (envelope [0 1 1 %] [0 0.15 0]) gate)
                   env (env-gen (env-perc 0.05 0.5) gate)
                   freq (* % dr 440)]
               (* (sin-osc (* f-env freq))
                  env))
            (range 2 5 1/4)
            (shuffle (n-range 0 1 20))))

;; 2018.02.25
(defsound test option
  (reduce (fn [acc x] (sin-osc (* acc x)))
          (lf-tri 2000)
          (take 12 (reductions * 10 (cycle [5/4 11/5 7/6])))))

;; 2018.02.26
(defsound test option-d
  (let [freq (dt 4 [440 4400 880])]
       (reduce (fn [acc x] (sin-osc (* acc freq x dr)))
               (sin-osc freq)
               (take 8 (cycle [2 0.33 8 1/2])))))

;; 2018.02.26
(defsound test option
  (detunr 0.01
             (fn [dr] (let [freq (dt 4 [440 4400 880])]
                        (reduce (fn [acc x] (sin-osc (* acc freq dr x)))
                                (sin-osc freq)
                                (take 5 (cycle [1/2 6 1 1 4])))))))

;; 2018.02.26(low)
(defsound test option
  (let [freq (env-gen (envelope [60 100 440 440] [4 4 4]) :action FREE)]
       (reduce (fn [acc [x y]] (sin-osc (* acc (sin-r x (- 1 y) (+ 1 y)) freq)))
               (sin-osc freq)
               [[1.3 0.05]
                [2.3 0.05]
                [0.7 0.1]
                [1.3 0.01]])))

;; 2018.02.27
(defsound test option
  (let [freq 440]
       (reduce (fn [acc x] (sin-osc (* x freq acc)))
               (sin-osc freq)
               [1/2 1/4 (sin-r 0.3 1/2 8) 9/4])))

;; 2018.02.28
(defsound test option
  (let [freq 440]
       (reduce (fn [acc [m1 m2]]
                 (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
               (sin-osc freq)
               [[1 2] [1/2 7] [1/4 7]])))

;; 2018.03.01
(defsound test option
  (let [freq 440]
       (reduce (fn [acc [m1 m2]]
                 (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
               (sin-osc freq)
               [[1/2 7]
                [1 (dt:kr 2 [2/7 3/7 2/3 4 3/2 3])]
                [1/4 (env-gen (envelope [7 7 1] [8 4]))]])))

;; 2018.03.07
(defsound test option-d
  (* (reduce (fn [acc x] (sin-osc (* acc x dr)))
                (gray-noise)
                [(dt:kr 4 [5 20 2000]) 2 128])
        (reduce (fn [acc x] (sin-osc (* acc x)))
                (sin-osc 440)
                [380 280 1440])))
;; 2018.03.09
(defsound test option-d
  (switch (lf-pulse (line:kr 40 1 12) 0 (sin-r:kr 0.27 0.5 1))
             (rg-exp (gray-noise) 0.5 1)
             (m-map #(sin-osc %)
                    [(rg-exp (gray-noise) 100 300)
                     (rg-exp (pink-noise) 800 1200)
                     (rg-exp (pink-noise) 3200 9600)])))

;; 2018.03.19
(defsound test option
  (let [freq (x-line [10 100] [1000 2000] 8)]
    (reduce (fn [acc [x y]] (ringz (sin-osc (* x acc)) y 0.2))
            (white-noise)
            [[freq 10] [110 2000] [440 100]])))

(defsound test option
  (let [freq (x-line [10 100] [1000 2000] 8)]
    (reduce (fn [acc [x y]] (ringz (sin-osc (* x acc)) y 0.2))
            (white-noise)
            [[110 2000] [440 100] [freq 10]])))

(defsound test option
  (let [freq (x-line [10 100] [1000 2000] 8)]
    (reduce (fn [acc [x y]] (ringz (sin-osc (* x acc)) y 0.2))
            (saw 220)
            [[440 100] [110 2000] [freq 10]])))

;; 2018.03.20
(defsound test option-d
  (m-map (fn [freq]
              (let [f-env (rg-lin (lf-pulse 1) 4/5 1)]
                (->
                 (white-noise)
                 (reduce-> (fn [acc x] (sin-osc (* acc x freq f-env))) [5/2 1 2])
                 (* 3 (env-gen (env-perc 0.05 8) (impulse 1/4)))
                 tanh)))
            [55 25 35 10]))

;; 2018.03.20 (like) pi po
(defsound test option
  (let [num 8]
       (splay (map (fn [freq phase freq2]
                     (let [gate (impulse 1/4 phase)
                           freq (rg-exp (lf-pulse freq phase) (* 1/10 freq2) freq2)]
                       (* num (sin-osc freq)
                          (env-gen (env-perc 0.05 2) gate))))
                   (n-range 1 10 num)
                   (shuffle (n-range 0 1 num))
                   (n-range 1000 10000 num)))))

;; 2018.03.21
(defsound test option-d
  (let [fact (x-line:kr 8 0.1 12)]
    (+ (* (sin-osc (* dr 160))
          (lf-pulse 1/2 0 (/ (- (* 3 fact) 1) (* 4 fact))))
       (* (sin-osc (* dr 240))
          (env-gen (env-perc 0 0.5) (impulse 1/2 1/4))))))

;; 2018.03.21
(defsound test option-d
  (let [gate (impulse 1)
        gate2 (impulse 1/4 5/8)
        freq 60
        f-env (env-gen (envelope [0 2.5 1] [1e-5 0.04]) gate)]
    (+ (tanh (* 1.2 (sin-osc (* freq f-env dr))
                (env-gen (envelope [0 1 0.5 0] [0 0.25 0.01]) gate)))
       (* 1/2 (tanh (* 8 (sin-osc (* freq f-env dr))
                       (env-gen (envelope [0 1 0.5 0] [0 0.125 0.08]) gate2))))
       (* (impulse 8)
          (lf-pulse 1/4 1/2 0.5))
       (* (impulse 2)
          (lf-pulse 1/6 1/4 0.25)))))

;; 2018.03.21
(defsound test option
  (+ (splay (-> (map (fn [freq1 freq2 i] (* (/ 1 i) (rlpf (lf-noise1 freq1) freq2 0.01)))
                     (n-range 100 600 8)
                     (shuffle (n-range 20 100 8))
                     (iterate inc 1))
                (* 8)
                tanh
                (* (env-gen (env-perc 0.05 2) (impulse 1/2)))))
     (detunr 0.01 (fn [dr] (let [gate (impulse 2)
                                 f-env (env-gen (envelope [0 3 1] [0.001 0.01]) gate)
                                 freq 60]
                             (-> (env-gen (env-perc 0.05 1) gate)
                                 (* (sin-osc (* freq f-env dr)))))))))

;; 2018.03.21
(defsound test option-d
  (m-map (fn [max-freq phase freq freq2]
           (-> (sin-osc (* dr (rg-exp (lf-saw freq) 10 max-freq)))
               (free-verb (env-gen (envelope [0 0 1] [4 4])) 1)
               (* (lf-pulse freq2 phase 0.25))))
         (cycle [480 2400 9600])
         (n-range 0 1 12)
         (cycle [42 60 320 210])
         (cycle [0.5 0.3 0.8 0.5 0.4])))

;; 2018.03.22
(defsound test option-d
  (let [base (rg-exp (lf-saw:kr 1/6 -1) 0.2 3)]
       (distort (* 8 (m-map (fn [freq] (-> (* (impulse (* base freq dr))
                                              (white-noise))
                                           (ringz 0 0.01)
                                           (lpf (* freq 1000))))
                            (n-range 0.5 4 8))))))

;; 2018.03.22
(let [num 12
      r1 (rg-lin (lf-saw:kr 0.3) 1/2 2)
      r2 (rg-exp (lf-saw:kr 0.7) 1/16 1)]
  (defsound test option
    (splay (map (fn [freq phase]
                     (* (sin-osc freq) 4
                        (lag-ud (lf-pulse (* r1 1.5) phase 1/4) 0.01 1e-1)
                        (lag-ud (lf-saw (* r2 3.5) phase) 0.01 1e-2)))
                   (shuffle (n-range 100 3000 num))
                   (n-range 0 1 num)))))

;; 2018.03.22
(defsound test option
  (splay (map #(let [f-env (env-gen (envelope [0 4 1] [0.005 0.01]) (impulse 1 %3))]
                    (* 8 (sin-osc (* %2 f-env (rg-lin (lf-pulse % 0 0.1) 40 50)))))
                 (n-range 1 2 8)
                 (n-range 1 4 8)
                 (shuffle (n-range 0 1 8)))))

;; 2018.03.23
(defsound test option
  (splay (map (fn [frq phase] (let [gate (impulse:kr (rg-exp (lf-saw:kr 0.3 phase) 0.01 8))
                                       freq (env-gen (envelope [0 4000 frq] [0 5e-3]) gate)]
                                   (-> (white-noise)
                                       (rlpf freq 0.001)
                                       (* 12 (env-gen (env-perc 0 0.04) gate)))))
                 (shuffle (n-range 200 2000 12))
                 (n-range 0 2 12))))


;; 2018.03.25
(defsound test option
  (-> (splay (map (fn [gate-freq freq]
                       (let [gate (impulse gate-freq)
                             f-env (env-gen (envelope [0 2 1] [0 0.01]) gate)]
                         (+ (* 1/4 (sin-osc (* 1/32 freq)) (saw freq)
                               (env-gen (envelope [0 1 1 0] [0 0.08 0]) gate))
                            (* (sin-osc (* 1/16 freq))
                               (env-gen (envelope [1e-8 1 0.5 1e-8] [0.05 0.2 0] :exp) gate))
                            (* 1/8 (rlpf (white-noise) (* freq f-env)
                                         0.4)
                               (env-gen (env-perc 0 0.3) gate)))))
                     (n-range 1/2 2 4)
                     [1760 880 1260 630]))
         (* 8)
         distort))

;; 2018.03.25
(defsound test option
  (+ (splay (map (fn [freq phase]
                      (let [gate (impulse 1 phase)]
                        (-> (sin-osc freq)
                            (* (sin-r 1/4 2 8))
                            (* (env-gen (env-perc 0.01 1) gate))
                            (* (lf-pulse 1/3 phase 3/4))
                            distort
                            (* 8))))
                    (shuffle (n-range 30 100 8))
                    (n-range 0 1 8)))
        (* (rlpf (white-noise) 8000 0.1)
           (env-gen (env-perc 0 0.05) (mix (impulse (switch (lf-pulse 1/3 0 3/4)
                                                            [3 4]
                                                            [1 2/3])))))))

;; 2018.04.06
(defsound test option-d
  (+ (let [gate (impulse 1)
           f-env (env-gen (envelope [0 8 1] [0 0.001]) gate)]
       (* (rlpf (white-noise) (* f-env 300 dr))
          (env-gen (env-perc 0.008 0.2) gate)))
     (let [gate (impulse 1/2 1/4)
           f-env (env-gen (envelope [0 8 1] [0 0.001]) gate)]
       (* (rlpf (white-noise) (* f-env 600 dr))
          (env-gen (env-perc 0.008 0.2) gate)))
     (let [gate (impulse 1/4 3/4)
           f-env (env-gen (envelope [0 5 1] [0 0.01]) gate)]
       (* (rlpf (white-noise) (* f-env 1200 dr) 0.8)
          (env-gen (env-perc 0.01 0.3) gate)))))

;; 2018.04.08
(defsound test option-d
  (* (let [freq (* dr 440)]
          (-> (m-map (fn [i] (let [d (/ 1 (inc i))
                                   freq (* freq (inc (* 2 i)))]
                               (* (sin-osc (sin-r freq (* 1/4 i freq) freq))
                                  (eff (line 0 1 12) (sin-r 4.6 0.5 1))
                                  d)))
                     (range 0 8))))
        (env-gen (envelope [1 1 1e-12] [6 6] :exp) :action FREE)))

;; 2018.04.09
(defsound test option
  (splay (map (fn [phase freq] (let [gate (impulse (dt:kr 2 [2 1 7]) phase)]
                                    (-> (white-noise)
                                        (rlpf  freq 0.3)
                                        (*  16 (env-gen (env-perc 0.005 0.1) gate))
                                        (tanh)
                                        (* 8))))
                 (n-range 0 1 16)
                 (n-range 440 940 16))))

;; 2018.04.10 window
(defsound test option-d
  (-> (switch (sin-osc (rg-exp (m-map lf-pulse [0.2 0.5 0.7]) 0.01 8))
                 (pink-noise)
                 (white-noise))
         (rlpf [(dt:kr 1/2 [100 150 180])
                (dt:kr 2 [1000 1500 1800])
                (rg-exp (m-map lf-pulse [0.8 1.1 1.3]) 100 2000)
                (rg-exp (m-map lf-saw [3.3 16.4 1.3]) 8000 9000)] 0.2)
         (* [1 1 1/2 1/6])
         (mix)))

;; 2018.04.11
(demo 12 (-> (switch (lf-saw (rg-exp (m-map lf-pulse [0.2 0.5 0.7]) 0.01 8))
                     (pink-noise)
                     (white-noise))
             (rlpf [(dt:kr 1/2 [100 150 180])
                    (dt:kr 2 [1000 1500 1800])
                    (rg-exp (m-map lf-pulse [0.8 1.1 1.3]) 100 2000)
                    (rg-exp (m-map lf-saw [3.3 16.4 1.3]) 8000 9000)]
                   (repeatedly 4 #(rg-exp (mix (lf-saw [0.15 1.2 0.5] (rand 3.14))) 1e-4 0.1)))
             (* [1 1 1/2 1/6])
             (mix)
             tanh))

;; 2018.07.06
(defsound test option
  (let [[x y] (splay (map #(* (sin-osc (* (sin-r 0.01 0.99 1.01) %))
                              (sin-r 0.3 0.5 1))
                          (take 32 (reductions * 100 (cycle [5/4 6/5 11/10])))))]
    (rotate2 x y (sin-r 0.1 -1 1))))


;; 2018.07.24
(defsound test option
  (splay (-> (* 8 (sin-r 0.4 0.9 1)
                (repeatedly 8 #(pulse (* 440 (sin-r 1.3 0.9999 1.0001))
                                      (rg-lin (lf-noise1 0.4) 0.1 0.9))))
             (free-verb 1 1))))

;; 2018.07.24
(defsound test option
  (splay (let [n 20]
           (-> (map #(* (+ (sin-osc 0.4 %) (sin-osc 0.08 %))
                        (sin-osc (* (* 400 (* (+ (sin-osc 0.1 %)
                                                 (sin-osc 0.13 %)
                                                 (* (sin-osc 0.7 %)
                                                    (sin-osc 0.2 %)))))
                                    (sin-osc (* %2 100)))))
                    (range 1 n)
                    (n-range 0 3 n))
               (* n 40)))))

;; 2018.07.24
(defsound test option-d
  (let [freq 1000]
    (* (reduce (fn [acc x] (+ acc (sin-osc (* x freq dr))))
               (take 8 (reductions * (cycle [1 1.3 1.2 0.9 0.8]))))
       (+ (reduce * (repeatedly 30 #(sin-osc (* 3.8 (sin-osc 0.02)))))
          (reduce * (repeatedly 30 #(sin-osc (* 1.8 (sin-osc 0.4)))))))))

;; 2018.07.24
(defsound test option-d
  (splay (map #(* 520 (reduce (fn [acc x] (* acc
                                             (sin-osc (* %2 687))
                                             (sin-osc 1287)
                                             (sin-osc (* %2 %2 33))))
                              (sin-osc % %2)
                              (range 1 16)))
              [(* 230 (sin-osc 0.3))
               (* 3 (sin-osc 0.1))
               (* 30 (sin-osc 1.3))
               (* 10000 (sin-osc 0.03))]
              (n-range 1e-5 1 4))))

;; 2018.07.31
(defsound test option-d
  (* 32 (splay
         (tanh (* 640 (let [freq 1000]
                        (map #(reduce (fn [acc x] (sin-osc (* acc % freq x)))
                                      (sin-osc (* % dr 800))
                                      [3.2 0.23 0.43 
                                       (* 20.2 % (sin-osc 0.834))
                                       (* 2.2 (sin-osc 3.734))
                                       (* 8.2 (sin-osc 0.134))])
                             (shuffle (n-range 1e-8 1 6)))))))))

;; 2018.07.31
(defsound test option-d
  (let [saw (fn [freq]
              (reduce (fn [acc x]
                        (let [phase 0.5
                              d (* (sin-osc 0.3 phase) 0.02)]
                          (+ acc
                             (* (/ 1 x)
                                (sin-osc (* (+ 1 (* 2 x) d) dr freq))))))
                      (sin-osc freq)
                      (range 2 16)))
        base-freq (+ 2300 (* (+ (sin-osc 0.37)
                                (sin-osc 1.37)
                                (sin-osc 0.2)) 3000))]
    (* 1/2 (saw (* base-freq (* (sin-osc 0.07) (saw 80))))
       (saw -0.8))))

;; 2018.07.31
(defsound test option-d
  (let [base-freq (+ 200 (* (sin-osc 0.02)
                            (sin-osc 0.02)
                            (sin-osc 0.02)
                            (sin-osc 0.021)
                            (sin-osc 0.02) 150))]
    (* 32 (splay (map #(* (sin-osc (* 1/8 %3) %3)
                          (sin-osc (* base-freq (+ (+ % %2) (* (+ (sin-osc (* %3 0.2) %3)
                                                                  (sin-osc (* %3 1) %3)
                                                                  (sin-osc (* %3 0.5) %3)) %)))))
                      (cycle [12 18 50 70])
                      (range 1 10)
                      (iterate #(* 1.2 %) 0.2))))))

;; 2018.07.31
(defsound test option
  (let [eff (+ 1 (sin-osc 0.002))]
    (* 20 (splay (mapcat (fn [x]
                           (map #(* (sin-osc 0.1 (* x %2 Math/PI)) (sin-osc (* (- x eff)  %)))
                                (range 150 1200 234)
                                (shuffle (range 0 2 0.2))))
                         (take 10 (iterate #(* 1.23 %) 1)))))))

;; 2018.07.31
(defsound test option
  (* 10 (splay (map #(* %1 (sin-osc %2) (sin-osc (* %3 600)))
                    (range 0 1 0.1)
                    (shuffle (range 0.01 0.05 0.002))
                    (shuffle (range 1 8))))))

;; 2018.08.04
(defsound test option
  (* 64 (splay (map (fn [gate freq]
                      (let [f-env (env-gen (envelope [0 2 1] [0 0.001]) gate)]
                        (* (bpf (brown-noise) (* f-env freq) 0.1)
                           (env-gen (env-perc 0.01 0.2) gate))))
                    (map #(impulse %1 %2) (cycle [1 1/2 1 1/4]) (n-range 0 1 16))
                    (reductions * 200 (cycle [1.6 1.2 0.7 0.95]))))))

;; 2018.08.04
(defsound test option
  (splay (map #(* (gray-noise)
                  (clip:ar (* 1/2 (+ (u/switch (lf-pulse (/ 1 %)) (lf-pulse %2) (sin-osc %2))
                                     (lf-saw %)))))
              (repeatedly 7 #(round (rg-exp (lf-noise0:kr 4) 1 16) 1))
              (cycle [0.3 1.2 8 0.1]))))

;; 2018.08.06
(defsound test option
  (splay (map #(let [main-gate (impulse 1/2 %)
                     gate (impulse (dq main-gate [1/2 2 %2]) 1/2)
                     vol (dq main-gate [1 1/8 1/6])
                     freq (* %3 (dq gate [2000 800 400]))]
                 (* vol 64
                    (env-gen (env-perc 0.01 (* %2 1/8 0.5)) gate)
                    (bpf (white-noise) freq 0.3)))
              (n-range 0 1 7)
              (shuffle (range 1 8))
              (repeatedly #(inc (rand 10))))))
;;  2018.08.07
(defsound test option
  (splay (map #(let [width (u/rg-exp (lf-saw:kr (+ 0.01 %)) 1/16 1/2)]
                 (* 32
                    (resonz (gray-noise)
                         (* %2 (u/rg-lin (lf-pulse 1 0 1/8) 1 3))
                         (u/rg-lin (lf-pulse 2) 0.4 1))
                    (lf-pulse 1 % width)))
              (u/n-range 0 1 8)
              (reductions * 1000 (cycle [1.1 1.5 0.6 0.28 3])))))
;; 2018.08.08
(defsound test option-d
  (let [base-freq (lin-lin (+ (sin-osc 0.3)
                              (lf-saw 0.5)) -2 2 200 2000)
        freq (* dr (u/switch (lf-pulse 1/4 0 (dt:kr 1 [1/2 1/8]))
                             (* (rg-exp (sin-osc (rg-exp (lf-tri 0.03) 20 500)) 0.5 1)
                                (+ (latch:ar base-freq (impulse (dt:kr 8 [2 16])))
                                   (u/rg-exp (m-map lf-pulse [0.7 0.3 0.23]) 50 1200)))
                             (* (sin-osc 110) (sin-osc base-freq) (rg-exp (lf-tri 0.23) 4000 5000))))]
    (sin-osc freq)))

;; 2018.08.08
(defsound test option-d
  (reduce (fn [acc x] (sin-osc (* x acc))) (sin-osc (* 100))
          [2000 8000 500 ]))

;; 2018.08.08
(defsound test option-d
  (reduce (fn [acc x] (sin-osc (* x acc))) (sin-osc (* 100))
          [2000 8000 (rg-lin (lf-pulse:kr (dt:kr 1/2 [[1/2 1/2 1/2 8]
                                                      [1/2 4 1/2 8]])
                                          (lf-saw:kr 18)) 800 300)]))

;; 2018.08.09
(defsound test option-d
  (let [env (m-map #(cubed (cubed (sin-osc %)))
                   [1/4 1/8 2])
        freq (rg-lin (lf-pulse (rg-exp (sin-osc:kr 0.08) 0.01 160)) 1000 5000)]
    (-> (* (bpf (white-noise) (* dr freq) env)
           (switch (lf-pulse 1/5) env (- 1 env)))
        (clip:ar 0.8 1)
        (leak-dc))))

;; 2018.08.09
(defsound test option
  (rotate2 (rlpf (white-noise) (rg-exp (sin-osc 0.057) 200 1800))
           (rlpf (white-noise) (rg-exp (sin-osc 0.03) 100 1000))
           (sin-osc (rg-exp (sin-osc 0.008) 0.03 1))))

;; 2018.08.10
(defsound test option-d
  (-> (+ (let [gate (impulse 9)
               freq (* dr (dq gate [1000 600 600 800 600 600]))
               vol (dq gate [1 1/2 1/2 1/2])
               release (dq gate [2 1 1])
               rq (dq (throttle gate 3) [0.3 0.5 0.8])]
           (* vol
              (bpf (white-noise) freq rq)
              (env-gen (envelope [0 1 0.5 0] [1e-5 0.01 (* release 0.05)]) gate)))
         (let [gate (impulse 6)
               vol (dq gate [1/4 (repeat 11 1/16)])
               freq (* dr (dq gate [4800 2400]))]
           (* (hpf (brown-noise) freq) vol
              (env-gen (env-perc 0 0.1) gate))))
      (* 64)
      clip:ar))

;;  2018.08.11
(defsound test option
  (let [pos (sin-osc 0.05)
        car-freq (rg-exp (lf-noise0 (rg-exp (sin-osc 0.1) 0.001 16)) 50 800)
        freq (rg-exp (sin-osc 0.1) 4000 4010)]
    (-> (pan-gau pos (sin-osc freq (rg-lin (sin-osc car-freq) 0 (* 2 Math/PI))))
        (free-verb 0.5 0.1))))

;;  2018.08.11
(defsound test option
  (-> (mm-map #(let [pos (sin-osc 0.0025 %)
                     freq (rg-exp (lf-noise0 16) %2 (* %2 1.1))
                     snd (reduce (fn [acc x] (sin-osc (* acc x %3)))
                                 (pulse freq)
                                 [500 300 1260])]
                 (pan-exp pos snd))
              (n-range 0 Math/PI 6)
              (reductions * 100 (cycle [8/7 11/10]))
              (iterate #(* 1.4 %) 1))
      (u/switch-> (!! b0) (* (lf-pulse 8 0 7/8)))
      (u/switch-> (!! b2) (free-verb 1 (!! (rand 1))))))

;; 2018.08.12
(defsound test option-d
  (let [env (clip:ar (+ (lf-pulse (dq (dust:kr 1/2) [36 36 32]) 0 (dq (dust:kr 1/4) [1/4 1/4 1/4 3/4]))
                        (lf-saw (dq (dust:kr 1/3) [-12 1 18 -4]))))]
    (* (rhpf (white-noise)
             (* dr (dq env [8000 6700 6700 6700 3350]))
             (dq env [0.5 0.2 0.8]))
       env)))





