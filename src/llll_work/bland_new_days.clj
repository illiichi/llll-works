(ns llll-works.bland-new-days
  (require [llll.core :as l4]
           [llll.clay.clay :as cl]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(connect-external-server "192.168.100.255" 57110)
(connect-external-server "192.168.100.255" 57110)

(kill-server)
(l4/finish)
(demo (sin-osc [660 1320]))
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

;; 2018.08.12
(defsound test option-d
  (rotate2 (* (sin-osc (* dr :-freq1))
              (env-gen (env-perc 0.05 0.5) (impulse :-tempo-1)))
           (* (sin-osc (* dr :-freq2))
              (env-gen (env-perc 0.05 0.5) (impulse :-tempo-2)))
           (sin-osc :-speed)))

(defpattern test-control
  {:table {:t1 (control :test :-tempo-1)
           :t2 (control :test :-tempo-2)
           :s (control :test :-speed)}}
  (&| (-> (| :s)
          (--> 1 >> 10 > [2] 2 > 1))
      (-> (| :t1)
          (--> 1 > [5] 2 - 1))
      (-> (| :t2)
          (--> 10 >> [5] 2  > 1))))

(defpattern test-control-2
  {:table {:f1 (control :test :-freq1)
           :f2 (control :test :-freq2)}
   :period 128}
  (&| (-> (| :f1)
          (--> 300 >> [20] 5000))
      (-> (| :f2)
          (--> 500 >> [20] 5000))))

;;  2018.08.13
(defsound test option-d
  (let [gate (impulse 1)
        f-env (* dr (env-gen (envelope [0 2.5 1] [0 0.01]) gate))]
    (-> (+ (* (sin-osc 40) (sin-osc (* (sin-osc (u/sin-r 2.3 30 30)) f-env 120))
              (env-gen (env-perc 0.001 0.5) gate))
           (let [env (env-gen (env-perc 0 0.15) gate)]
             (* (bpf (gray-noise) (* f-env (u/sin-r 5.2 500 520)) 0.8) 1/32
                (u/eff (- 1 env) (lf-noise1 45))
                env)))
        (* 10 (lf-tri 70))
        tanh
        (lpf (* f-env 800)))))

;; 2018.08.15
(defsound test option
  (pan-az 8 (map #(let [gate (impulse 2 %)
                        freq (midicps %2)
                        f-env (env-gen:kr (envelope [0 8 2 8] [0 0.08 1]) gate)]
                    (* (lpf (u/m-map lf-tri (take 8 (reductions * freq (cycle [17/10 14/13 13/8]))))
                            (* freq f-env))
                       (env-gen (env-perc 0.0001 1/2) gate)))
                 (u/n-range 0 1 16)
                 (mapcat #(chord % :major) [:F5 :A6 :G5] ))
          (u/rg-lin (sin-osc 2.3) -2 2)))

;; 2018.08.15
(defsound test option-d
  (splay (map (fn [f gate]
                (let [freq (* dr (u/dq gate [f (* 3/2 f)]))]
                  (u/m-map #(let [f-env (env-gen (envelope [0 4 1 1.1] [0 0.005 1]) gate)]
                              (ringz (* (gray-noise) (env-gen (env-perc 0 0.01) gate))
                                     (* f-env %) 0.2))
                           (take 8 (reductions * freq (cycle [7/4 13/10]))))))
              [1200 800 600 900 770]
              (map impulse
                   [3/2 1 1/4 2 1/9]
                   [0 1/2 1/4 1/4 0]))))

;; 2018.08.15
(defsound test option-d
  (u/m-map (fn [gate freq]
             (* 8 (u/m-map #(* %2 (sin-osc (* % freq (u/dq gate [1 1 1 1 2 4]) dr)))
                         (->> (map *
                                   (take 16 (reductions + 1 (reductions * 1 (repeat 1.15))))
                                   (cycle [1 0 0 1 1 1 1]))
                              (filter #(< 1e-3 %)))
                         (iterate #(* 1/2 %) 1))
                (env-gen (env-perc 0.01 0.25) gate)))
           (map #(impulse % %2) (cycle [1 3/2 5/3]) (u/n-range 0 1 4))
           (reductions * 300 (cycle [5/4 11/8]))))

;; 2018.08.16
(defsound test option-d
  (let [snd (u/m-map (fn [n]
                       (saw (* dr
                               (u/rg-exp (u/m-map lf-saw (map #(* n %) [1 4 6]))
                                         12.5 (* n 50)))))
                     (take 3 (iterate #(* 1.05 %) 1)))]
    (ringz snd (+ (u/rg-exp (lf-tri (u/dt:kr 8 [12 1 4 0.1])) 500 1200)
                  (u/rg-exp (lf-pulse (u/dt:kr 4 [8 1])) -100 400)) 0.01)))

;; 2018.08.16
(defsound test option-d
  (-> (* (lag (lf-pulse 1 0 1/8) 0.001)
         (+ (lpf (saw (* dr 40)) 100)
            (sin-osc (u/rg-lin (sin-osc 0.3) 10 25))))
      (free-verb 0.3 0.2 0.01)))

;; 2018.08.17
(defsound test option-d
  (let [freq (* dr (u/dt:kr 4 [80 120 180 280]))
        freq2 (u/rg-exp (u/m-map lf-pulse [3.4 0.7 0.6]) 50 120)]
    (-> (* (sin-osc (* (sin-osc 120) freq)) (saw (u/rg-exp (sin-osc 70) freq2 (* 2 freq2))))
        (u/reduce-> (fn [acc x] (+ acc (* 1/2
                                          (allpass-c acc 0.2 (u/rg-lin (lf-noise1 8) 0.01 0.2) x))))
                    [0.4 0.1 0.2 0.01])
        (lpf (u/rg-exp (sin-osc 0.08) 400 10000))
        tanh)))

;; 2018.08.17
(defsound test option-d
  (-> (lf-tri (* dr
                 (u/rg-lin (lf-pulse 8) 600 900)
                 (u/rg-exp (sin-osc 74) 0.1 4)))
      (lpf (u/rg-exp (sin-osc 0.08) 400 10000))
      (free-verb 1 (u/sin-r 0.05 0.1 1))))

;; 2018.08.17
(defsound test option
  (-> (lf-tri (* (u/rg-lin (lf-pulse 8) 600 900)
                 (u/rg-exp (sin-osc 74) 0.1 4)))
      (lpf (u/rg-exp (sin-osc 0.08) 400 10000))
      (g-verb)))

;; 2018.08.21
(defsound test option
  (-> (map (fn [freq env] (* env (blip freq)))
           [50 2000]
           [(lf-tri 1/8) (lf-saw 4)])
      (u/reduce->  (fn [acc x] (* (lf-tri x) (free-verb acc 1 1)))
                   [180 18 3 8])
      (* 30)
      tanh))

;; 2018.08.21
(defsound test option
  (let [base (u/sin-r 0.01 1 12)
        rq (u/sin-r 0.32 0.01 0.05)]
    (-> (splay (map #(let [base-freq %
                           freq (u/switch (lf-pulse base-freq 0 (u/sin-r:kr 0.3 1/16 3/4))
                                          (u/rg-exp (lf-saw (* 2 base-freq) (rand)) 100 10000)
                                          (u/rg-exp (sin-osc (* 1/4 base-freq) (rand)) 100 2000))]
                       (-> (gray-noise)
                           (ringz (* base freq) rq)))
                    (u/n-range 0.01 1 16)))
        (clip:ar))))

(defsound test option-d
  (-> (splay (map #(let [gate (impulse (* :-ratio %2) %3)
                         freq (* % (env-gen (envelope [0 1 100 10] [0 0.0001 0]) gate))
                         env (env-gen (envelope [0 1 1 0] [0 0.05 0.03]) gate)]
                     (* env (rlpf (white-noise) (* dr freq) 0.1)))
                  (iterate #(* 1.6823 %) 50)
                  (u/n-range 1 2 10)
                  (shuffle (u/n-range 0 1/4 10))))
      (* 32)
      (distort)
      (tanh)))

(defpattern test-control
  {:table {:f1 (control :test :-ratio)}
   :period 128}
  (-> (| :f1)
      (--> 1 >> [20] 4 [10] 1/8 [3])))

;; 2018.08.25
(defsound test option-d
  (-> (white-noise)
      (u/reduce-> (fn [acc x] (+ acc (ringz (delay-n acc 0.5 (u/rg-lin (lf-noise0 8) 0.08 0.5))
                                            x (u/rg-lin (lf-noise0 4) 0.01 0.5))))
                  (take 12 (iterate #(* 1.28 %) (u/rg-exp (lf-tri 0.005) 50 1000))))))

;; 2018.08.25
(defsound test option-d
  (-> (white-noise)
      (u/reduce-> (fn [acc x] (+ acc (* 1/2 (ringz (delay-n acc 0.5 (u/rg-lin (lf-noise0 8) 0.08 0.5))
                                                   x (u/rg-lin (lf-noise0 4) 0.01 0.5)))))
                  (take 5 (iterate #(* 1.28 %) (u/rg-exp (lf-tri 0.005) 50 1000))))))

;; 2018.08.25
(defsound test option-d
  (-> (white-noise)
      (u/reduce-> (fn [acc x] (+ acc (ringz (allpass-n acc 0.5 (u/rg-lin (lf-noise0 8) 0.08 0.5) 0.1)
                                            x (u/rg-lin (lf-noise0 4) 0.01 0.5))))
                  (take 8 (iterate #(* 1.28 %) 500)))))

;; 2018.08.25
(defsound test option-d
  (-> (white-noise)
      (u/reduce-> (fn [acc [x y]] (+ acc (* (ringz (allpass-n acc 0.5 (u/rg-lin (lf-noise0 8) 0.08 0.5) 0.1)
                                                   x (u/rg-lin (lf-noise0 4) 0.01 0.5))
                                            (env-gen (env-perc 0.2 4) (impulse 1/4 y)))))
                  (take 8 (iterate #(* 1.28 %) 500))
                  (shuffle (u/n-range 0 1 8)))))

;; 2018.08.26
(defsound test option
  (-> (splay (reductions (fn [acc x]
                           (+ acc
                              (* (u/sin-r 0.02 0 1)
                                 (delay-l acc x (* (u/sin-r 0.03 0 1) x)))))
                         (saw :-freq)
                         (u/n-range 0.01 0.5 8)))
      (hpf 400)
      (* 10)
      (distort)))

(defpattern test-control
  {:table {:f1 (control :test :-freq)}
   :period 128}
  (-> (| :f1)
      (--> 100 [10] >> 10 [20] >> 1000)))

;; 2018.08.26
(defsound test option
  (-> (splay (* (reductions (fn [acc x] (+ acc (* 1/2 (delay-n acc x x))))
                            (saw (u/rg-exp (pink-noise) 10 300))
                            (u/n-range 0.001 0.01 8))
                (map #(env-gen (env-perc 0.05 0.5) (impulse 2 %))
                     (u/n-range 0 1 8))))
      (hpf 200)
      (* 10)
      (distort)))

;; 2018.08.26
(defsound test option
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


(defsound test option
  (+ (-> (splay (map #(let [gate (dust 0.8)
                            freq (+ (midicps (round (latch:ar (u/sin-r 1.08 %2 (* 3 %2)) gate) 1))
                                    (u/sin-r 3.3 0.9 1))]
                        (-> (ringz (* (white-noise) gate) freq 0.1)
                            (free-verb 1 1)
                            (free-verb 1 1)))
                     (u/n-range 0 1 8)
                     (shuffle (u/n-range 50 80 8))))
         (* 128)
         (distort)
         (lpf (u/sin-r 0.2 300 1500)))
     (* (u/sin-r 0.08 0.25 1) (sin-osc 240)
        (sin-osc (* 80 (sin-osc 43))))))

(defsound test option
  (+ (-> (splay (map (fn [x] (let [gate (impulse (u/dt:kr 2 [4 4 1 1/8]) x)
                                   f-env (env-gen (envelope [0 8 1] [0.0001 0.1]) gate)
                                   freq (* f-env
                                           (latch:ar (midicps (u/m-map #(round (u/sin-r 0.3 % %2) 1)
                                                                       (scale :F4 :major)
                                                                       (scale :F5 :major)))
                                                     gate)
                                           (u/rg-lin (lf-pulse 8) 1 3/2))
                                   env (env-gen (env-perc 0.001 0.08) gate)]
                               (* env (sin-osc freq))))
                     (u/n-range 0 1 8)))
         (lpf (u/rg-exp (sin-osc 0.08) 100 2000))
         (* 800)
         (tanh)
         (* 1/4))
     (splay (map #(let [gate (lf-pulse 1/4 0 3/4)]
                    (* 8 gate (rlpf (saw %)
                                  (u/rg-exp (sin-osc 0.08 (rand 3)) 100 10000))))
                 (u/n-range 20 120 8)))
     (u/m-map #(let [gate (impulse %2)
                     f-env (env-gen (envelope [0 2 1] [0 0.0001]) gate)]
                 (* (u/dq gate [4 2 1]) (bpf (gray-noise) (* f-env %1))
                    (env-gen (env-perc 0 %3) gate)))
              [(u/sin-r 0.08 1000 8000) 7000 100]
              [16
               (u/dt:kr 3 [14 6])
               (u/dt:kr 2 [8 48])]
              [0.01 0.02 0.1])))


(defsynth-l4 test-synth
  [freq 440 long 1]
  (let [f-env (env-gen (envelope [0 4 1] [0.001 0.18]))
        dur (* long dur)]
    (+ (pan2 (* (sin-osc freq)
                (env-gen (env-perc 0.08 dur) :action FREE)))
       (-> (repeatedly 5 #(saw (* freq (u/rg-exp (pink-noise) 0.995 1.005))))
           (lpf (* freq f-env))
           (* (env-gen (env-perc 0.01 dur)))))))

(defpattern test-pattern
  {:table {:a (synth test-synth)}
   :period 64}
  (&| (map #(->> (| :a :a|5)
                 (=| :long [2 1.1])
                 (=| :freq (map midi->hz %1)))
           [[77 79] [77 72] ])))

;; 2018.09.08
(defsound test option
  (let [min-freq (u/rg-exp (sin-osc 0.008) 3 500)
        max-freq (u/rg-lin (lf-noise0 1/8) 1200 4800)
        snd (u/m-map #(* (sin-osc (+ min-freq (* %1 max-freq)))
                         (env-gen (env-perc 0.05 0.5) (impulse 1 %))
                         (env-gen (env-perc 0.0025 0.05) (impulse 16)))
                     (u/n-range 0 1 8))
        pan (u/rg-lin (sin-osc 0.01) 0 1)]
    [(* pan snd)
     (* (- 1 pan) snd)]))

;; 2018.09.10
(defsound test option
  (splay (let [f (lag-ud (u/m-map #(u/rg-lin (lf-pulse %) 0 %2)
                                  [8 2 16 3 7 1/2]
                                  [50 180 32 66 82 100])
                         0.02 0.1)
               gate-freq (u/rg-exp (lf-saw:kr 0.01) 0.05 10)]
           (map (fn [freq phase]
                  (let [gate (impulse gate-freq phase)]
                    (* (sin-osc (+ f freq))
                       (env-gen (env-perc 0.05 0.5) gate))))
                (concat (reverse (u/n-range 880 4800 8))
                        (u/n-range 880 4800 8))
                (u/n-range 0 1 16)))))

;; 2018.09.18
(defsound test option
  (let [gate (impulse (u/dt:kr 1 [1 1 1 1 1 4]))
        freq (-> (u/dq gate (concat [440 640 940]
                                    [440 640 940]
                                    [440 840 1880]
                                    [440 1680 280]))
                 (lag-ud 0.2 0.4))
        snd (reduce #(* %1 (lf-tri %2))
                    (sin-osc freq)
                    [12 18 -1/4])
        [x y] (splay (map #(-> snd (free-verb % %2))
                          (shuffle (u/n-range 0 1 8))
                          (u/n-range 0 1 8)))]
    (rotate2 x y (sin-osc 0.2))))

(defsound test option
  (let [eff (+ 1 (sin-osc 0.002))]
    (* 20 (splay (mapcat (fn [x]
                           (map #(* (sin-osc 0.1 (* x %2 Math/PI)) (sin-osc (* (- x eff)  %)))
                                (range 150 1200 234)
                                (shuffle (range 0 2 0.2))))
                         (take 10 (iterate #(* 1.23 %) 1)))))))
