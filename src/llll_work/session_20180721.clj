(ns llll-works.session-20180721
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

(def option {:swap-option {:switch-dur 16}
             :state {:initial (update-state {:count 0})
                     :update update-state}})
(def option-d (merge option {:synth-option {:type :detune}}))


(defsound test option
  (mm-map #(let [pos (rg-lin (lf-tri %2 %3) -2 2)]
             (pan-gau pos (sin-osc (* :-freq %))))
          (range 1 8)
          (map #(/ 1 %) (range 3 11))
          (shuffle (n-range 0 2 8))))

(defsound test option-d
  (mm-map #(let [pos (rg-lin (lf-tri %2 %3) -2 2)]
                (pan-lin pos (sin-osc (* 440 % dr))))
             (range 1 8)
             (map #(/ 1 %) (range 3 11))
             (shuffle (n-range 0 2 8))))

(defsound test option                   ;flow
  (mm-map #(let [gate (impulse 1/8 %)
                 freq (* :-freq %2)
                 pos (env-gen:ar (envelope [0 1 1 -1 -1 1] [0 1/2 3/2 1/2 3/2]) gate)
                 vol (env-gen:ar (envelope [0 1 0 1/6 0] [1 1 1 1]) gate)]
             (pan-exp pos (* vol (sin-osc freq))))
          (take 16 (reductions #(mod (* % %2) 1) 1 (cycle [1/3 7/4])))
          (reductions * 1 (cycle [11/9 13/11 22/36]))))

(defpattern test-pattern
  {:table {:a (control :test :-freq)}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32}
  (--> (| :a*8)
       440
       > [3] (if (= (mod count 2) 0) 880 3520)
       [2]
       >|    (if (= (mod count 3) 0) 2520 300)))



(defpattern test-pattern
  {:table {:a (control :test :-freq)}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32}
  (--> (| :a*16)
       220
       > [4] (if (= (mod count 2) 0) 880 3520)
       220
       > [2] (if (= (mod count 4) 0) 880 1760)
       [3]
       >|    (if (= (mod count 3) 0) 2520 300)))


(defsound test option-d
  (-> (switch (sin-osc (rg-exp (m-map lf-pulse [0.2 0.5 0.7]) 0.01 8))
              (pink-noise)
              (gray-noise))
      (rlpf [(dt:kr 1/2 [100 150 180])
             (dt:kr 1/4 [1000 1500 1800])
             (rg-exp (m-map lf-pulse [0.8 1.1 1.3]) 100 2000)
             (rg-exp (m-map lf-saw [3.3 16.4 1.3]) 400 800)]
            0.8)
      (* [1 1 1/2 1/6])
      (mix)
      (* 64 (lf-pulse 1/2 0 1/16))
      (u/reduce-> (fn [acc [x y]] (* (sin-osc y) (free-verb acc x 1)))
                  [1 1 1 0.8]
                  [300 2200 370 100])
      (tanh)))


(defsound test option-d
  (+ (m-map (fn [freq] (* (sin-osc freq) (sin-r (sin-r 0.1 (* :-min-r freq) (* :-max-r freq)) 0.3 1)
                          (clip:ar (sin-r 0.18 0.1 1) 0.1 0.5)))
            (flatten [ (take 10 (iterate #(* % 1.2) 1200))
                      (take 6 (iterate #(* % 1.5) 1800))
                      (take 4 (iterate #(* % 2.4) 700))]))))

(l4/control :test :-max-r {:dur 64 :to 2})
(l4/control :test :-min-r {:set 1/8})

(defpattern test-pattern
  {:table {:a (control :test :-freq)}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32}
  (--> (| :a*16)
       220
       > [4] (if (= (mod count 2) 0) 880 3520)
       220
       > [2] (if (= (mod count 4) 0) 880 1760)
       [3]
       >|    (if (= (mod count 3) 0) 2520 300)))


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

(defsound test option-d
  (let [gate (impulse 1)
        f-env (env-gen (envelope [0 2 1 1 2 1] [0 0.08 0.1 0.08 0.2]) gate)]
    (switch (lf-pulse (dt:kr 2 [[1/3 1/3 1/3 8]
                                [1/3 2 1/3 2]])
                      0 1/8)
            (lf-tri (* f-env 6000))
            (tanh (* 32 (m-map #(let [freq (* f-env %)
                                      am-freq (* %2 freq)
                                      am-freq2 (* 1/5 am-freq)]
                                  (* (sin-osc freq) (sin-osc (* am-freq (sin-osc am-freq2)))))
                               (reduce (fn [acc x] (cons (* (first acc) x) acc))
                                       [2800 800]
                                       (take 10 (cycle [1.5 1.1 1.1])))
                               (cycle [1/4 1/8 1/16])))))))


(defsound test option
  (splay (map #(let [freq %2
                     freq2 (env-gen (envelope [0 10 100 700] [0 0.5 2.3]) (impulse 1/4 %))]
                 (* 8 (sin-osc (* freq (sin-osc (* freq 1/2 (sin-osc freq2)))))
                    (lf-pulse 1/8 %)))
              (n-range 0 1 20)
              (take 20 (iterate #(* 1.2 %) 500)))))

(defsound test option-d
  (let [n 12
        gate (impulse 1)
        freq 180]
    (* (m-map #(* 6 % (sin-osc (* dr %2 freq)))
            (map #(/ 1 %) (range 1 n))
            (map #(Math/pow 0.987 (* % %)) (range 2 (inc 12))))
       (env-gen (env-perc 0.05 0.5) gate))))

(defsound test option-d
  (let [freq (* dr :-freq)
        f-env (env-gen (envelope [0 2.3 0.4] [0.18 1])
                       (impulse (dt:kr 4 [0.8 8 1/2 1/2])))]
    (-> (m-map (fn [ratio] (m-map #(* 8 (sin-osc (* ratio % freq))
                                      (sin-r f-env 0.1 1)
                                      (/ 1 %))
                                  (range 1 8 2)))
               [1 8/3 5/3])
        (tanh)
        (free-verb 0.7 10))))



(l4/control :test :vol {:dur 32 :to 0.5})

(l4/control :test :-freq {:set 110})
(l4/control :test :-freq {:dur 128 :to 27.5})
(l4/control :test :-freq {:set 220})
(l4/control :test :-freq {:set 440})
(l4/control :test :-freq {:set 880})
(l4/control :test :-freq {:set 3520})
(l4/control :test :-freq {:dur 32 :to 240})

(defpattern test-pattern
  {:table {:a (control :test :-freq)}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32}
  (--> (| :a*8)
       (* 110 (inc (mod count 4)))
       > [4] (if (= (mod count 2) 0) 880 3520)
       (* 55 (inc (mod count 8)))
       > [2] (if (= (mod count 4) 0) 880 1760)
       [(inc (mod count 8))]
       >|    (if (= (mod count 3) 0) 2520 300)))



(defsound test option
  (let [max-note (env-gen (envelope [0 90 120] [0 1.5] :step) (impulse 1/2))]
    (splay (map #(let [gate (impulse %2 %)
                       freq (latch:ar (midicps (round (rg-lin (sin-osc 1.3) 30 max-note) 1)) gate)]
                   (* 16 (sin-osc (rg-lin (gray-noise) freq (* 1.08 freq)))
                      (env-gen (env-perc 0.01 0.8) gate)))
                (n-range 0 1 16)
                (n-range 1/4 1.5 16)))))

(l4/control :test :vol {:dur 128 :to 0})

(defsound test2 option
  (splay (map #(let [gate (* (impulse 16) (env-gen (envelope [0 1 1 0] [0 1/16 0]) (dust:kr %)))
               freq (latch:kr (lin-lin (mix [(sin-osc:kr 0.1)
                                             (lf-pulse:kr 8)]) -1 1 %3 %4) gate)]
           (* (sin-osc freq)
              (env-gen (env-perc 0.01 %2) gate)))
        [3 1 2 0.5]
        [0.15 0.8 0.08 1]
        [400 1800 8000 100]
        [1200 2000 12000 300])))

(l4/control :test2 :vol {:dur 16 :to 0})

(l4/control :test2 :vol {:dur 128 :to 1})

(l4/control :test :vol {:dur 128 :to 1})



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


(defsound test option-d
  (let [freq (env-gen (envelope [220 240 1200 400] [4 2 4]))]
    (tanh (m-map #(let [gate (impulse %2 (rand))
                        vol (dq gate [1 1/2 1/4 1/4])]
                    (* (/ 1 %) (sin-osc (* % (latch freq gate))) vol 8
                       (env-gen (env-perc 0.01 0.4) gate)))
                 (take 12 (iterate #(* 1.28 %) 1))
                 (n-range 1 4 12)))))

(l4/control :test :vol {:dur 128 :to 0})

(defsound test option
  (let [freq2 (rg-exp (m-map lf-pulse [1/3 0.8 0.4]) 100 8000)
        freq3 (rg-exp (m-map lf-saw [1/3 0.4 8]) 10 3000)]
    (-> (switch (gray-noise)
                (distort (leak-dc (sin-osc (* 440 (saw (* freq2 (sin-osc freq3)))))))
                (rlpf (lf-noise0 1000) freq2 0.3))
        (* (lf-pulse :-pulse-freq 0 (dt:kr 3 [1/8 7/8])))
        (u/reduce-> (fn [acc x] (ringz acc x 0.01))
                    [280 600 :-max-freq]))))

(defpattern test-pattern
  {:table {:a (control :test :-max-freq)
           :b (control :test :-pulse-freq)}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32}
  (&|
   (if (= (mod count 4) 0)
     (--> (| :b*8)
          16 - 128 - 1)
     (--> (| :b*12)
          8 - 1 - (if (= (mod count 4) 0) 1 16) - 4))
   (if (= (mod count 5) 0)
     (--> (| :a) 2000 > 8000)
     (--> (| :a*8)
          (* 110 (inc (mod count 4)))
          > [4] (if (= (mod count 2) 0) 880 7040)
          (* 55 (inc (mod count 8)))
          > [2] (if (= (mod count 4) 0) 880 1760)
          [(inc (mod count 8))]
          >|    (if (= (mod count 3) 0) 2520 300)))))


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

(defsound test2 option-d
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


(defsound test option
  (let [max-note (env-gen (envelope [0 90 120] [0 1.5] :step) (impulse 1/2))]
    (splay (map #(let [gate (impulse %2 %)
                 freq (latch:ar (midicps (round (rg-lin (sin-osc 1.3) 30 max-note) 1)) gate)]
             (* 16 (sin-osc (rg-lin (gray-noise) freq (* 1.08 freq)))
                (env-gen (env-perc 0.01 0.8) gate)))
          (n-range 0 1 16)
          (n-range 1/4 1.5 16)))))

(defsound test3 option-d
  (m-map #(let [gate (impulse %2 %)
                freq (latch:ar (midicps (round (rg-lin (sin-osc 1.3) 70 90) 1)) gate)]
            (* 4 (sin-osc (rg-lin (gray-noise) freq (* dr 1.08 freq)))
               (env-gen (env-perc 0.01 0.8) gate)))
         (n-range 0 1 16)
         (n-range 1/4 1.5 16)))

(defsound test2 option
  (m-map #(let [freq (rg-exp (lf-saw (dt:kr 2 [-1/4 25 80]) %2) 100 %)]
               (rlpf (white-noise) freq 0.01))
            (n-range 1000 8000 8)
            (n-range -1 0 8)))

(defsound test2 option
  (let [num 8]
       (splay (map (fn [freq phase freq2]
                     (let [gate (impulse 1/4 phase)
                           freq (rg-exp (lf-pulse freq phase) (* 1/10 freq2) freq2)]
                       (* num (sin-osc freq)
                          (env-gen (env-perc 0.05 2) gate))))
                   (n-range 1 10 num)
                   (shuffle (n-range 0 1 num))
                   (n-range 1000 10000 num)))))

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

(l4/control :test :vol {:dur 64 :to 1})

(defsound test option
  (splay (map #(let [gate (impulse 1/5 %2)
               freq (latch (* (dt:kr 4 (n-range 440 880 4)) %) gate)]
           (* (sin-osc (* freq ))
              (eff (env-gen (envelope [0 0 0 1] [0 2 2]) gate) (sin-r 3.3 0 0.5))
              (env-gen (envelope [0 1 1 0] [1 3 1]) gate)))
        (range 1 8)
        (reverse (n-range 1/4 1 8)))))

(l4/control :test :vol {:dur 16 :to 0})

(defsound test2 option-d
  (m-map #(let [freq (sin-r 0.3 100 102)
                   gate (impulse 1/10 %2)]
               (* (sin-osc (* dr % freq (env-gen (envelope [0 1 3 3 1.5] [0 2 2 1/4]) gate)))
                  (env-gen (env-perc 2 10) gate)))
            (concat (take 8 (iterate #(* 3/2 %) 1))
                    (take 8 (iterate #(* 5/3 %) 1)))
            (concat (n-range 1/4 1/2 8)
                    (n-range 3/4 1 8))))

(l4/control :test2 :vol {:dur 16 :to 0})

(l4/control :test2 :vol {:dur 16 :to 0.1})
