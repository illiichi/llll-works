(ns llll-works.good-old-old-days
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

;; 2015.05.04
(defsound test option
  (let [snd (partial map (fn [i]
                           (let [freq (lin-exp (clip:ar (sin-osc (mod i 3)
                                                                 (/ i Math/PI)) 0 0.5)
                                               0 0.5
                                               (* i 100) (* i 500))
                                 snd (map #(* (sin-osc (* freq (/ %1 %2)))
                                              (lf-tri i))
                                          (range 3 6)
                                          (range 5 12 3))]
                             (mix snd))))]
    [(mix (snd (range 8 20 3)))
     (mix (snd (range 4 12 2)))]))

;; 2015.05.04
(defsound test option-d
  (let [source-ugen white-noise]
    (letfn [(f [rythm phase length f-arr v-arr f-env]
              (let [pulse (impulse:kr rythm phase)
                    env (env-gen (env-perc 0 length) pulse)
                    f-mult (dq pulse f-arr)
                    vol (dq pulse v-arr)
                    fenv (env-gen f-env pulse)
                    freq (* 440 f-mult fenv)]
                (* (rlpf (source-ugen) freq 0.5) env vol 16)))]
      (mix [(f 1   0   0.25  [5 3 4 3] [1 1/2]       (envelope [8 1 3] [0.01 0.05]))
            (f 4   0   0.1   [1 1/2] [1 1/2 3/4 3/4] (envelope [1/4 3 1] [0.01 0.3]))
            (f 2   1/4 0.03  [8 4] [1/8 1/4]         (envelope [1/4 1 8] [0.01 0.3]))
            (f 8   0   0.03  [8 4] [1/16 1/8]        (envelope [1/4 1 8] [0.01 0.3]))
            (f 1/2 1/2 1     [1/2] [2]               (envelope [1/4 8 1] [0.05 0.2]))
            (f 16  1/2 0.03  [8 4] (concat (repeat 8 0)
                                           (repeat 8 1/4)) (envelope [2 1] [0.01]))]))))

;; 2015.05.05
(defsound test option-d
  (let [pulse (impulse:ar (dt 0.5 [16 8 4 8]))
           base-freq (-> (latch:ar (sin-osc 0.1) pulse)
                         (rg-lin 440 880))
           base-freq (x-line:ar 200 4000 40)
           vol (line 3 0.6 40)
           arr (->> (slice 1 7)
                    (map (fn [[x y]] (/ x y))))
           iarr (->> arr (map #(/ 1 %)))
           freq-ratio [1
                       (dq pulse arr)
                       (dq pulse iarr)]]
       (mix (* (sin-osc (map #(* base-freq %) freq-ratio))
               vol))))
;; 2015.05.06
(defsound test option
  (splay (let [f (fn [base phase]
                   (let [arr (sort [3/2 5/3 9/5 7/4 8/5 4/3 5/4 3])
                         bump (abs (sin-osc:kr (rg-lin (lf-saw:kr 1/60 1) 0 1)))
                         pulse (impulse:kr (* bump 16))
                         index (rg-lin (sin-osc:kr 0.1) 0 (count arr))
                         fenv (select:kr (* bump index) arr)
                         freq (rg-lin  (sin-osc 0.01) base (* base 2))
                         env (env-gen (env-perc 0.01 0.2) pulse)
                         switch (env-gen (envelope [0 1 1 0] [1 3 1])
                                         (lf-pulse 1/20 phase 1/10))
                         freq (latch:ar (* fenv freq switch) pulse)]
                     (* (sin-osc freq)
                        env
                        switch
                        6)))
               arr (shuffle (range 100 1000 100))]
           (map f arr
                (iterate #(+ (/ 1 (count arr)) %) 0)))))

;; 2015.05.07
(defsound test option-d
  (let [f (fn [dur level freq]
               (let [duty-dur (map #(/ % dur) [1/4 1/2 1/4])
                     bound (dt (dseq duty-dur INF) (dseq [1 level 1] INF))
                     freq (lin-lin (wrap:ar (lf-saw 1) (* -1 bound) bound) -1 1
                                   freq (* freq 3 dr))]
                 freq))]
       (mix [(sin-osc (f 1 0.2 440) 0)
             (* (square  (f 6 0.1 30)) 1/2)
             (sin-osc (f 3 0.8 30))
             (sin-osc (f 4 0.1 1000) 0.3)
             (* (saw (f 4 0.2 3000)) 1/4)
             (sin-osc (f 8 0.3 800) 0.3)])))

;; 2015.05.08
(defsound test option-d
  (let [sound (* (sin-osc (+ 400 (* 1200 (lf-noise0 4) dr)))
                    (squared (abs (lf-tri:ar 0.5))))
           filtered (reduce (fn [acc x] (allpass-c acc 0.2 (+ 0.01 (rand 0.04)) 1))
                            sound
                            (repeatedly 5 #(rand 0.04)))]
       (* 2 (+ filtered sound))))

;; 2015.05.08
(defsound test option
  (map (fn [a phase] (let [pulse (impulse 1 phase)
                              sound (* (env-gen (env-perc 0 1) pulse)
                                       (white-noise)
                                       2)]
                          (rlpf sound
                                (rg-exp (sin-osc:kr (rg-exp (lf-tri:kr a) 1/100 10)) 100 10000)
                                (rg-exp (sin-osc:kr 10) 1/10 1))))
          [0.1 0.401] [0 1/4]))

;; 2015.05.08
(defsound test option
  (splay (pulse (map #(rg-lin (lf-saw 0.1 %) 440 1760)
               (range -1 1 0.2)))))

;; 2015.05.08
(defsound test option
  (let [f (fn [phase]
               (let [trig (impulse:ar 1/3 phase)
                     freq (latch:ar (lin-exp (white-noise) 0 1 300 4000) trig)
                     env (env-gen:ar (envelope [0 1 1 0] [1 1 1]) trig)
                     sound (lpf (+ (* (pulse (+ (/ freq 2) (* 10 (sin-osc 0.2)))) 1/2)
                                   (* (sin-osc (+ (/ freq 1) (* 2 (sin-osc 2)))) 3/2)
                                   (* (saw (+ (* freq 4) (* 200 (sin-osc 0.05)))) 1/6))
                                (* freq 5))]
                 (* env sound)))]
    (splay (map f (range 0 1 1/5)))))

;; 2015.05.09
(defsound test option
  (let [trig (impulse (duty:kr (dseq [3 1] INF) 0 (dseq [1 4 1 2 1 4 1 2 8 1] INF)))
           freq [(dq trig [200 250 180 300])
                 (dq trig [150 252 120 304 100 248 120 308] )]
           env (env-gen (envelope [0 1 1 0] [0.02 0.5 0.02]) trig)]
       (* (lpf (+ (pulse (rg-lin (sin-osc 2) freq (* 1.01 freq)))
                  (* (sin-osc (rg-lin (sin-osc 8) (/ freq 9/4) (/ freq 5/2))) 1/2))
               (* freq 4))
          env)))

;; 2015.05.09
(defsound test option
  (let [n 20]
    (splay (map #(let [phase %1
                       freq (rg-lin (lf-tri %2 phase) %3 (* %3 %4))
                       env (max (lf-tri 0.01 phase) 0)]
                   (* 3 (sin-osc freq) env))
                (range 0 4 (/ 4 n))
                (repeatedly n #(ranged-rand 0.001 0.005))
                (repeatedly n #(exp-rand 10 40))
                (repeatedly n #(exp-rand 3 300))))))

;; 2015.05.09
(defsound test option-d
  (let [n 10
        snd (mix (map (fn [phase vib freq-base freq-ratio]
                        (let [freq (* dr
                                    (rg-lin (sin-osc 1/8 phase) (- 1 vib) (+ 1 vib))
                                    (rg-lin (lf-tri 0.005 phase)
                                            freq-base (* freq-base freq-ratio)))
                              env (max (lf-tri 1/60 phase) 0)]
                          (* (sin-osc freq) env 8)))
                      (range 0 4 (/ 4 n))
                      (repeatedly n #(exp-rand 0.01 0.05))
                      (repeatedly n #(exp-rand 100 1000))
                      (repeatedly n #(exp-rand 1 3))))
        width (dt 1  [0.7 1 0.7 1 0.2 0.1 1])
        env (lag (lf-pulse:ar 5 0 width) 0.01)]
    (* snd env)))

;; 2015.05.10
(defsound test option
  (let [ratio (rg-lin (white-noise) (x-line 0.01 0.5 60) (x-line 0.1 1 60))
           trig  (impulse (x-line:kr 1/4 10 60))
           freq  (lag-ud (latch:ar (lin-lin ratio 0 1 400 3000) trig) 3 3)]
    (splay (* (sin-osc [freq
                  (rg-lin (lf-tri 1) (* freq 5/4) (* freq 4/3))
                  (* freq 3/2)])
        [(rg-lin (sin-osc 1/2) 1/2 1)
         1
         (rg-lin (sin-osc 1/2) 1 1/3)]))))

;; 2015.05.11
(defsound test option-d
  (let [freq (rg-lin (lf-tri 0.1) 50 60)
           rfreq (rg-lin (sin-osc 0.2) 250 380)
           fb (rg-lin (lf-saw 1) 0.01 0.8)]
       (* (+ (* 4 (lpf (pulse freq) rfreq))
             (sin-osc-fb (* 2 dr freq) fb))
          (lag-ud (lf-pulse:ar (dt 1 [1 2 4 8])
                               0
                               (dt 4 [1 0.5 0.1]))
                  0.02 0.02))))

;; 2015.05.11
(defsound test option
  (splay (map #(* (saw (* 5 %))
                   (lf-pulse 0.1 (* % 4/10) 0.2)
                   8)
               (range 1 10))))
;; 2015.05.11
(defsound test option
  (saw (dt (x-line:kr 1 0.001 20) (range 10 50 2))))

;; 2015.05.11
(defsound test option
  (let [arr (map / (drop 1 (primes 20)) (primes 20))
        n 10]
    (splay (map #(let [trig (impulse 0.2 %2)
                       freq %1]
                   (square (* freq (lag (dq trig arr)) 3)))
                (shuffle (range 100 3000 (/ n 3000)))
                (map #(/ (mod % 4) n) (range 0 12))))))

;; 2015.05.11
(defsound test option-d
  (rlpf (latch:ar (saw (* dr (dt 10 [100 800 811 8001]))) (impulse 400))
           (rg-exp (lf-saw:kr 0.1 1) 100 20000)
           0.5))

;; 2015.05.11
(defsound test option-d
  (rlpf (latch:ar (white-noise) (impulse:ar (dt 10 [30 100 400])))
        (* dr (rg-exp (lf-tri:kr 0.5 1) 100 20000))
        (dt 5 [0.8 0.1])))

;; 2015.05.11
(defsound test option-d
  (let [base-freq (dt 1/4 [500 431 440 340 620 460])
        freq (* dr base-freq (dt 3 [1 5/4 7/3]))]
    (+ (* (sin-osc freq) 1/4)
       (* (sin-osc (+ (rg-lin (sin-osc 1) -100 100) (* freq 4))) 1/16)
       (* (saw (+ (rg-lin (sin-osc 10) 0 100) (* freq 8))) 1/16)
       (* (saw (+ (rg-lin (sin-osc 50) -50 50) (* freq 8))) 1/8))))

;; 2015.05.12
(defsound test option
  (splay (map (fn [phase vol rythm freq attack decay]
                (let [trig (impulse rythm phase)]
                  (* vol 90 (bpf (white-noise) freq 0.1)
                     (env-gen (env-perc attack decay) trig))))
              (range 0 1 1/10)
              [0.5 0.05 0.551 0.705 0.432 0.938 1.0 1.5 0.627 1]
              [1/8 4 1/3 1/3 1/3 1 1 1 1 12]
              [3000 1534 819 1037 7610 982 628 717 320 250]
              [0.1 0.05 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
              [1 0.3 0.074 1.212 0.094 0.257 0.235 0.003 0.322 0.1])))

;; 2015.05.13 (サーバーが落ちる? s.options.memSize = 16 * 8192 で再起動)
(defsound test option
  (let [arr (map int "we love music")]
    (mix (map #(+ (* (pulse (+ (* % 2) (* 5 (dt 1/8 arr))))
                     (max 0 (lf-saw 0.1 (/ % 2 (count arr)))))
                  (g-verb (* 1/4
                             (square (* 8 (dt (/ (mod % 4) 8) arr)))
                             (lf-pulse 0.2 (/ % (count arr))))))
              arr))))

;; 2015.05.14 (2回目がおかしい？サーバーを再起動すると直る)
(defsound test option
  (let [base (dt 3 [400 200 3000])
           freq (rg-lin (lf-pulse 8) base (* base 7/5))]
       (* (mix (sin-osc (* freq [1 3/2 8])))
          (lf-pulse:ar (dt 1 (take 3 (iterate #(* 2 %) 4)))))))

;; 2015.05.15
(defsound test option-d
  (let [base (dt 1/16 [300 320 300 320 300 320
                          800 700 600 500
                          400 700 900 500 400])
           factor (rg-lin (lf-noise0 1/4) 1/2 5)
           freq (* base factor dr)
           trig (lf-pulse 0.25)
           fenv (+ 1 (env-gen (adsr 0.5 0.1 0.3) trig))
           env (env-gen (adsr) trig)]
       (rlpf (* (saw freq) env)
             (* freq fenv)
             0.1)))

;; 2015.05.15
(let [arr (vec (filter #(and (< 2/3 %) (< % 3/2) (not (= 1 %)))
                       (set (for [i (primes 30)
                                  j (primes 30)] (/ i j)))))]
  (defsound test option-d
    (let [base (latch:ar (rg-lin (lf-noise2 1) 700 1500) (impulse 2))
          trig (lf-pulse 8)
          trig2 (lf-pulse (rg-lin (lf-pulse 1/4) 1 4))
          factor (+ (* trig (demand trig2 0 (drand arr INF)))
                    (* (- 1 trig) 1))
          freq (* base dr factor)]
      (sin-osc freq))))

;; 2015.05.15
(let [arr (vec (filter #(and (< 2/3 %) (< % 3/2) (not (= 1 %)))
                       (set (for [i (primes 30)
                                  j (primes 30)] (/ i j)))))]
  (defsound test option-d
    (let [base (latch:ar (rg-lin (lf-noise2 1) 700 1500) (impulse 2))
             trig (lf-pulse 4 [0 0.5])
             trig2 (lf-pulse (rg-lin (lf-pulse 1/4) 1 4))
             factor (demand trig2 0 (drand arr INF))
             freq [base (* dr base factor)]
             env (env-gen (env-perc 0.03 0.15) trig)]
         (mix (* 3/2 (sin-osc freq) env)))))

;; 2015.05.16
(defsound test option-d
  (let [freq (* dr (rg-exp (lf-saw:kr 1 3) 200 6000)
                (rg-lin (lf-saw:kr (dt:kr 1 [5 10 30 100])) 5/10 1))]
       (rlpf (white-noise) freq 0.2)))

;; 2015.05.16
(defsound test option
  (let [f (fn [phase freq ratio]
               (let [ph (* phase 2 Math/PI)
                     vib (rg-lin (sin-osc 1/4 ph) 0.97 1.03)
                     tr  (rg-lin (sin-osc 5 ph) 0.9 1.01)
                     fenv (* freq
                             vib
                             (lin-lin (max 0 (sin-osc 1/9 ph)) 0 1 1 ratio))]
                 (* (sin-osc fenv) tr)))
           trig (dt 5 [1 1/2 1/8 1/16 10])]
    (splay (map f
          (range 0 1 1/18)
          (iterate #(* 5/4 %) (dt trig [800 500 400 300 700 50]))
          (repeat 10/9)))))

;; 2015.05.17 (AM)
(defsound test option-d
  (let [trig (impulse 1/15)
           freq (rg-lin (lf-noise0 1/15) 400 2000)
           freq2 (* dr freq (eg-seq0 [14/30 1/2 4325/8000] 3 trig))]
       (* 2/3
          (sin-osc freq)
          (+ 1 (sin-osc freq2)))))

;; 2015.05.17 (FM?)
(defsound test option-d
  (let [v (dt 20/8 [20 30 60])
           fm (dt 20/8 [10 30 70 800])
           freq (dt 1/8 (map #(+ 60 (* 10 %)) (range 20)))]
       (saw (+ freq (* dr (sin-osc fm) v)))))

;; 2015.05.17 (FM?)
(defsound test option-d
  (let [v (dt 20/8 [20 30 60])
           fm (dt 20/8 [10 30 70 800])
           freq (dt 1/8 (map #(+ 200 (* 50 %)) (range 20)))]
       (pulse (+ freq (* dr (sin-osc fm) v))
              (rg-lin (sin-osc 0.1) 0.1 0.5))))

;; 2015.05.18
(defsound test option
  (splay (map #(* (dt 3 [1 8])
                  (pulse %2 (rg-lin (lf-tri 10) 0.1 0.5))
                  (env-gen (env-perc %4 %3) (impulse 1 %1)))
              (range 0 1 1/10)
              [400 222.753 301.742 593.132 399.957
               711.529 221.222 618.61 536.128 354.939]
              [0.069 0.166 0.203 0.232 0.263 0.305 0.498 0.615 0.469 0.365]
              [0.025 0.025 0.025 0.025 0.05 0.006 0.006 0.006 0.013 0.125])))

;; 2015.05.18
(defsound test option
  (splay (map #(tanh (* 20
                        (rlpf (white-noise) %5)
                        (lf-pulse:ar %2)
                        (env-gen (env-perc %3 %4) (impulse 4 %1))))
              (repeatedly 5 #(rand 1.0))
              (repeatedly #(exp-rand 0.1 1000))
              (repeatedly #(exp-rand 0.01 0.1))
              (repeatedly #(exp-rand 0.01 0.1))
              (repeatedly #(exp-rand 50 8000)))))

;; 2015.05.18
(defsound test option-d
  (lpf (* (mix (map #(bpf (white-noise) (* dr %) 0.08)
                       (map #(dt (a2k (dt 2 [1/2 1/4 1/8 1/16])) %)
                            (transpose [
                                        [400 2000 2550]  ; i
                                        [530 1850 2500]  ; e
                                        [620 1200 2400]  ; u
                                        [300 870 2250]   ; oo
                                        ;;                                         [270 2300 3000] ; ee
                                        [660 1700 2400]]))))
             20)
          3000))

;; 2015.05.19
(defsound test option
  (splay (map #(let [trig (impulse 2 %2)
                     env (env-gen (envelope [0 1 0.8 0] [0.2 0.1 0.5]) trig)
                     fenv (+ 1 (env-gen (envelope %1 [0.05 0.025 0.05]) trig))
                     freq (dq trig [200 250 310 500])]
                 (* (rlpf (saw (* freq fenv)) (* 2 freq fenv) 0.4)
                    5 env))
              (repeatedly 5 (fn [] (repeatedly 4 #(ranged-rand 0.05 1))))
              (repeatedly 5 #(rand 0.5)))))

;; 2015.05.21
(defsound test option
  (splay (map (fn [base-phase base-freq]
                (mix (map #(let [freq (lin-lin (clip2 (sin-osc 0.05 %1) 0.5)
                                               -0.5 0.5 %2 (* 5/4 %2))
                                 trig (impulse 1 %1)
                                 env (eg-seq0 [0.2 1] 0.5 trig)]
                             (* (sin-osc freq) env 4))
                          (map #(+ base-phase %) [0 0.16 0.32])
                          (map #(* base-freq %) [1 3/2 2]))))
              (range 0 Math/PI (* Math/PI 1/4))
              [150 200 520 660])))

;; 2015.05.22
(defsound test option-d
  (let [trig (impulse 1/6 [0 0.3])
        env1 (env-gen (envelope [0 1 0] [0.3 0.4]) trig)
        env2 (* (eg-seq [0 0 -0.2 -1] 1 trig))
        fenv1 (env-gen (envelope [0 1 0] [0.1 0.3]) trig)
        fenv2 (* (env-gen (envelope [0 0 1] [0.4 1.6]) trig)
                 (sin-osc 5) 1/8)
        base-freq [(dt 6 [300 800 200 400 2600])
                   (dt 1 [500 200 2600 800])]
        freq (+ base-freq (* dr base-freq (+ fenv1 fenv2)))]
    (* 1/2 (saw freq)
       (+ 1 env1 env2))))

;; 2015.05.25
(defsound test option
  (splay (map #(let [rythm 1/6
               ratio (dt rythm [1 3/2 5/4 7/4 9/2])
               fvol  (dt rythm [1 (* 1/2 %2) (* 1/4 %2) %2])
               trig (impulse 1/10 %1)
               freq  (+ (env-gen (envelope [400 2000] [10]) trig)
                        4
                        (* fvol (sin-osc 100)))]
           (sin-osc (* ratio freq)))
        (range 0.1 0.5 0.1)
        [100 300 200 100 2400])))

;; 2015.05.25
(defsound test option
  (splay (map (fn [vol phase fvol]
                (let  [l (rg-exp (sin-osc:kr 0.1) 0.1 30)
                       p (lf-pulse 1/10)
                       m (dt 5/2 [1200 400])
                       freq (+ (* p       (rg-lin (lf-saw l phase) 200 m))
                               (* (- 1 p) (rg-lin (lf-saw l phase) m 200) ))
                       ffreq (* 2 freq)]
                  (* 4 vol (sin-osc (+ freq (* fvol (sin-osc ffreq)))))))
              (reverse (range 0.1 1 1/10))
              (range 0 1 1/10)
              (range 100 8000 100))))
;; 2015.05.26
(defsound test option
  (splay (map #(let [trig (impulse:ar (dt 5 [2 1 0.25 32]) %1)
                     env (env-gen:ar (env-perc 0.01 0.122) trig)
                     base-freq (dq trig [500 450 480 580 530])
                     fvol %2
                     ffreq (* 2/3 base-freq)
                     freq (+ base-freq (* fvol (sin-osc ffreq)))]
                 (* 8 (sin-osc freq) env (- 1 (* %1 2))))
              (range 0.1 0.5 0.05)
              (reverse [10 100 500 2000 4000
                        8000 200 2000 8000 800]))))

;; 2015.05.26
(defsound test option
  (splay (map #(let [trig (lf-pulse 1/4 %1)
               fenv (env-gen (envelope [0 1 0 1/8 0 1/4 0]
                                       [1/8 1/4 1/4 1/8 1/8 1/4]) trig)
               env (env-gen (adsr 0.4 0.4) trig)
               freq %2]
           (leak-dc (* (sin-osc (* (rg-lin (lf-saw -1 %1) 1 freq)
                                   (sin-osc (* (lin-lin fenv 0 1 0.026 1/4) freq))))
                       env)))
        (range 0 0.5 1/10)
        [800 3000 2000 4500 1800])))

;; 2015.05.28
(defsound test option-d
  (* (rg-lin (lf-noise0:kr [32 8]) 0.3 2)
     (rlpf (white-noise) (* dr (rg-exp (lf-noise0:kr 3) 200 4000)))
     (lf-pulse:ar (rg-exp (lf-saw:kr 0.1 -1) 0.01 1000))))

;; 2015.05.31 (like) water
(defsound test option
  (splay (apply map #(* 4 (ringz (* (impulse %3 %4)
                                       (white-noise))
                                    %1 %2))
                   (transpose [[400 0.3 1 0]
                               [600 0.1 1/2 0.1]
                               [550 0.15 1/2 0.15]
                               [1200 0.8 1/4 0.5]
                               [1800 0.4 1/4 0.51]
                               [260 0.8 1/4 0]
                               [180 2   1/4 0.05]]))))

;; 2015.06.01
(defsound test option
  (let [freq [(rg-lin (lf-pulse 16) 400 1200)
              (rg-lin (lf-saw 1) 300 2000)]]
       (* 1/4 (+ (* 1/2 (pulse freq (rg-lin (lf-noise0 30) 0.1 0.5)))
                 (* (resonz (white-noise) (rg-lin (sin-osc 1) 100 2000) 2)
                    (- 1 (cubed (lf-saw 1/5))))))))

;; 2015.06.01
(defsound test option
  (let [freq [(rg-lin (sin-osc 0.1) 100 200)
              (rg-lin (sin-osc 0.11) 100 200)
              (rg-lin (sin-osc 1) 140 180)
              (rg-lin (sin-osc 1/2) 40 190)]]
    (* 1/32 (splay (hpf (ringz (saw freq) freq 0.8) 300)))))

;; 2015.06.02
(defsound test option-d
  (let [trig (impulse:ar 1/2)
        trig2 (impulse:ar 1/2 0.5)]
    (+ (sos:ar trig 0 0.055 0 1.99 -0.999)
       (* (sin-osc (* dr 650)) (env-gen:ar (env-perc 0 0.28) trig2)))))

;; 2015.06.04
(defsound test option
  (let [freq (map #(+ (demand (impulse:kr %)
                                 0
                                 (dibrown 500 1500 100))
                         (sin-osc %))
                     [1/4 1 2 4 10 12])
           trig (lf-pulse 1)]
       (splay (+ (* trig (sin-osc freq))
                 (* 2 (- 1 trig) (pulse freq))))))


;; 2015.06.05
(defsound test option-d
  (let [arr (flatten [(repeat 2 [1000 1800 500 800 1100])
                      (range 300 800 100)
                      (repeat 2 [1000 2000 2200 800 1100])
                      (reverse (range 1000 5000 1000))
                      (range 300 1600 200)])
        rythm [3/2 3/8]
        env (rg-lin (lf-saw (* -1 rythm) 1) 0 1)
        trig (impulse rythm)
        freq-low (dq trig arr)
        freq-high (dq trig (rotate 1 arr))
        p (+ freq-low (* dr (- 1 (lf-pulse rythm 0 0.12))
                         (- freq-high freq-low)))]
    (* (sin-osc p) env)))

;; 2015.06.06
(defsound test option
  (map (fn [rythm]
            (let [trig (impulse rythm)
                  base (dq trig (shuffle [100 150 220 400 900 2000]))
                  freq (+ base (* base 1/40 (sin-osc 0.2)))
                  freq (map #(* freq %) [1 3.5 6 8 13])
                  vib-freq (rg-lin (sin-osc 1) (eg-seq0 [1 0.8 0.5] 0.3 trig) 1)
                  rfreq (* base (eg-seq0 [4 2 1.5 1] 0.2 trig))]
              (* (rlpf (mix (sin-osc freq)) rfreq) vib-freq)))
          [1/2 1/4]))

;; 2015.06.06
(defsound test option
  (let [freq-high (line:kr 800 4000 20)
           freq (* (lag (lin-exp (duty 1/4 0 (dbrown 0 1 0.05)) 0 1 220 freq-high) 0.15)
                   (rg-lin (sin-osc 2) 0.99 1))
           diff (* freq 9/19)
           diff-vol 0.5
           diff2 (* diff (line 5/4 4 20))
           diff-vol2 (* diff-vol (dt 1 [1/2 1/2 2/3 4 10 100]))
           a (rg-lin (sin-osc 1/4) 0 0.5)]
    (splay (* (sin-osc [freq
                  (+ freq diff) (- freq diff)
                  (+ freq diff2) (- freq diff2)])
        [(- 1 (* 1/2 a))
         (* (+ 0.5 a) diff-vol)  (* (+ 1 (* -1 a)) diff-vol)
         (* (+ 0.5 a) diff-vol2) (* (+ 1 (* -1 a)) diff-vol2)]))))
;; 2015.06.06
(defsound test option
  (splay (map (fn [freq phase t]
                   (let [trig (impulse (/ 1 t) phase)]
                     (* (rlpf (pulse freq) (line freq (* 4 freq) 1))
                        (env-gen (env-perc 0.15 4) trig))))
                 (range 400 3000 500)
                 (repeatedly #(rand 1))
                 (repeatedly #(choose [4 3 10])))))

;; 2015.06.07
(defsound test option
  (let [base-freq [120 160]
        mod-freq [(rg-lin (sin-osc 1/3) 10 60)
                  (rg-lin (sin-osc 1/8) 20 100)
                  (rg-lin (sin-osc 1/5) 60 80)]
        index (rg-lin (lf-tri 1/4) 1 8)]
    (* 8 (mix (lpf (clip2 (pm-osc base-freq mod-freq index) 0.8) 150)))))

;; 2015.06.07
(defsound test option
  (* 8 (lpf (pulse 50 (lin-lin [(sin-osc (x-line 80 0.01 10))
                                (lf-tri 1/4)] -1 1 0.1 0.5)) 300)))

;; 2015.06.08
(defsound test option
  (let [freqs [440 660 1200 300 100 800 4000 130 50]]
       (splay (map #(let [trig (impulse 1/2 %2)
                          fenv (env-gen (envelope [0 0 1 0] [0.15 0.4 0]) trig)
                          fenv (lin-lin fenv 0 1 % (* 2 %))
                          env  (env-gen (env-perc 0.05 0.5) trig)]
                      (* 16 (sin-osc fenv) env))
                   freqs
                   (range 0 1 (/ 1 (count freqs)))))))

;; 2015.06.08 (moon)
(defsound test option
  (splay (let [freqs (range 400 2000 312)]
              (map (fn [freq phase]
                     (let [env (min (sin-osc 0.1 phase) 0)]
                       (* (sin-osc (dt 10 [freq (* 3/2 freq)])) env)))
                   freqs
                   (range 0 Math/PI (/ Math/PI (count freqs)))))))

;; 2015.06.09
(defsound test option
  (let [freqs (range 400 2000 212)
           base [(dt 2 [400 900 8000 4000 2000 1200])
                 (dt 2 [600 800 6000 4800 600 8000])]
           base-phase [0 Math/PI]]
       (mix (map (fn [freq phase]
                   (let [env (min (sin-osc 1 (+ base-phase phase)) 0)
                         trig (< env 0)
                         fenv (env-gen:ar (envelope [0 0 1 0] [0.15 0.4 0]) trig)
                         fenv (lin-lin fenv 0 1 freq (* 2 freq))]
                     (* (sin-osc (+ fenv base)) env)))
                 freqs
                 (range 0 Math/PI (/ Math/PI (count freqs)))))))

;; 2015.06.09
(defsound test option
  (let [freqs [440 660 1200 300 100 800 4000 130 50]]
       (splay (map #(let [trig (impulse 1/2 %2)
                          fenv (env-gen (envelope [0 0 1 0] [0.15 0.4 0]) trig)
                          fenv (lin-lin fenv 0 1 % (* 2 %))
                          env  (env-gen (env-perc 0.05 0.5) trig)]
                      (* 16 (pulse fenv (rg-lin (sin-osc (line 1 10 15)) 0.1 0.5)) env))
                   freqs
                   (range 0 1 (/ 1 (count freqs)))))))

;; 2015.06.10
(defsound test option
  (let [trig (impulse:ar (dt 1 [1 21 1]))
           fenv (lin-lin (env-gen:ar (envelope [1 1 0] [0.1 0.1]) trig) 0 1 1 8)
           ratio [1 3/2 5/4 1/2 4]
           base-freq (dq trig [800 550 1220])
           freq (* base-freq fenv ratio)
           env (env-gen:ar (env-perc 0.05 0.5) trig)
           snd (resonz (white-noise) freq 0.05)
           snd2 (* (saw (/ base-freq 10)) 1/3)]
       (splay (* 16 (g-verb (+ snd snd2) 10 0.5 1 1) env))))

;; 2015.06.10 (like) cute?
(defsound test option
  (splay (map #(let [rythm (dt 1 [1 4 2 1/2 3 3/2])
                     trig (impulse:ar rythm %)
                     freq (demand trig 0
                                  (dibrown 200 1200 50))]
                 (* (sin-osc freq)
                    10 (env-gen:ar (envelope [0 1 0]
                                             [(/ 2/3 rythm) 0.05])
                                   trig)))
              (range 0 0.8 0.2))))

;; 2015.06.10 (like) rythm? or not
(defsound test option-d
  (let [rythm (dt (a2k (x-line 2 0.1 5)) [-5 -8 2 -15])
        freq (* dr (rg-lin (lf-saw rythm 1) 100 250))
        env (rg-lin (lf-saw rythm 1) 0 1)]
    (* (mix (map #(* %2 (sin-osc (* %1 freq)))
                 [1 1/2 2/3 4/5 3/8]
                 [2 1   1   1/2 1/4])) env)))

;; 2015.06.11
(defsound test option
  (let [trig (mk-rythm 0.01 [3 0.1 0.1 2
                             0.01 0.01 0.01 0.01])
        arr [3000 8000 3200 3800 4000 20000]
        ratio (dq trig [1 1/10])
        fenv (+ [400 200]
                (* 4 (sin-osc 1/4))
                (* ratio arr (env-gen:ar (env-perc 0.02 0.5) trig)))]
    (splay (* 6 [1 4] (lpf (pulse fenv) 2000)))))

;; 2015.06.12
(defsound test option
  (splay (map #(let [trig (impulse 1 %)
                     fenv (rg-lin (lf-tri 100) 10 %2)
                     c (rg-lin (white-noise) %3 1)]
                 (* 4 (pulse fenv) c
                    (env-gen (env-perc 0 %4) trig)))
              (repeatedly 5 #(rand 1))
              (repeatedly 5 #(+ 50 (rand 3000)))
              (range 0.3 0.8 0.1)
              (repeatedly 5 #(rand 0.5)))))

;; 2015.06.12
(defsound test option
  (let [arr (* 1/3 (primes 8))
           p [ (mix (lf-pulse arr))
              (mix (lf-pulse arr 0.5))]
           trig (- (last-value p 0.001) p)
           env (env-gen (env-perc 1 0.1) trig)
           max-freq (env-gen (envelope [1000 1000 5000 300] [6 6 2]))
           freq (lag-ud (lin-lin p 0 1 (* 1/5 max-freq) max-freq) 0.2)
           r-range (rg-lin (lf-saw 0.1) 2 5)]
       (* env (rlpf (pulse freq) (* freq r-range)))))

;; 2015.06.13
(defsound test option
  (splay
      (map #(let [trig (impulse %2)
                  trig2 (impulse 1 %)
                  freq (lin-lin (demand trig 0 (dibrown 0 100 %3))
                                -0 100 200 2000)
                  env (env-gen (env-perc 0.05 %4) trig2)]
              (* 10 env (sin-osc freq)))
           (range 0 1 0.2)
           [4 4 8 1/4 16]
           [5 10 5 10 5]
           [0.8 0.8 0.8 2 0.2])))


;; 2015.06.13
(defsound test option
  (splay (apply map #(let [trig (impulse %1 %2)
                           freq %5
                           fenv (env-gen (envelope [1 3 1] [0.05 0.1]) trig)
                           env (env-gen (env-perc %3 %4) trig)]
                       (* 24 %6 (rlpf (saw freq) (* freq fenv)) env))
                (transpose [ [1   0    0.05 0.5  440 1/2]
                            [1   1/2  0.05 0.5  600 1/2]
                            [1/3 1/2  0.05 0.5  200 2]
                            [1/2 2/4  0.05 0.5  500 2]
                            [1/2 3/4  0.05 0.5  720 3/2]
                            [1/4 1/10 0.1  0.7  880 2]
                            [1/6 3/10 0  1.5  1600 2]
                            [4   0    0.03 0.05 900 2/3]]))))

(l4/stop :test)

;; 2015.06.14(low)
(defsound test option-d
  (let [trig (impulse 2)
           fenv (env-gen (envelope [800 150 100] [0.01 0.05]) trig)]
       (mix (* [4 2 4 1/4 1/8]
               (sin-osc [fenv 111 52 200])
               (env-gen (envelope [0 1 1 0] [0.005 1/8 0.01]) trig)))))

;; 2015.06.14
(defsound test option
  (splay
      (map #(let [trig (impulse 1 %)
                  freq (rg-lin (white-noise) 100 %2)
                  car-env (env-gen:ar
                           (envelope [(* freq 1/2) 0 (* freq 1/4)]
                                     [0.01 0.1]) trig)
                  m (line 3 30 20)]
              (* 8
                 (pm-osc freq car-env (rg-lin (lf-tri m) 1 10))
                 (env-gen (env-perc 0.05 %3) trig)))
           (repeatedly 5 #(rand 1))
           (repeatedly 5 #(+ 50 (rand 3000)))
           (range 0.3 0.8 0.1))))

;; 2015.06.14
(defsound test option
  (splay (map #(let [trig (impulse 1 %)
                        fenv (rg-lin (lf-tri 100) 10 %2)
                        c (rg-lin (white-noise) %3 1)]
                    (* 4 (clip2 (pulse fenv) c)
                       (env-gen (env-perc 0 %4) trig)))
                 (repeatedly 5 #(rand 1))
                 (repeatedly 5 #(+ 50 (rand 3000)))
                 (range 0.3 0.8 0.1)
                 (repeatedly 5 #(rand 0.5)))))

;; 2015.06.15
(defsound test option
  (map #(let [trig (mk-rythm 0.2 [1 1/3 1/3 1/2 1/4 1/4])
                 f-min (dq trig [1/2 1/5 1/10 1/10])
                 fenv (lin-exp (env-gen (envelope [0 1 f-min]
                                                  [0.1 0.2]) trig)
                               0 1 0.1 10)
                 fenv2 (* %3 (sin-osc 1/2))
                 env (env-gen (adsr 0.01 0.3 0.5 0.15) trig)]
             (* 3 env (rlpf (saw (+ fenv2 %1)) (* fenv %2) 1)))
          [82.1 110]
          [115  320]
          [30 60]))

;; 2015.06.15
(defsound test option
  (splay
      (map #(let [trig (mk-rythm 0.01 [1/2 1/2 1 1/4 1/4 1/4 1/4 2])
                  freq (dq trig [% (* 5/4 %2 %) % (* 3/2 %3 %) %])
                  fenv (env-gen (envelope [0 1 1 1/4] [0.01 0.15 0.2]) trig)
                  env (env-gen (env-perc 0.05 0.3) trig)
                  c (rg-lin (lf-tri 10) 0.1 0.9)]
              (g-verb (* env (pulse (* fenv freq) c)) 0.8 3))
           [4000 4200 4150 4158]
           [1.2 1.1 1.8 0.9]
           [0.9 1.3 0.5 0.9])))

;; 2015.06.16
(defsound test option
  (splay (map #(let [rythm (dt 4 [1 2 1 4])
                        trig (impulse:ar rythm)
                        freq (env-gen:ar (envelope % (repeat 3 (/ 1 rythm 4)))
                                         trig)
                        env (env-gen:ar (env-perc 0.05 0.5) trig)]
                    (* 4 env (bpf (rlpf (white-noise) freq 0.1) freq 0.5)))
                 (transpose [ [400 2000 2550]
                             [530 1850 2500]
                             [620 1200 2400]
                             [300 870 2250]]))))

;; 2015.06.17 (like) formant
(defsound test option
  (splay (map #(let [freq (env-gen:ar (envelope % (repeat 6 2)))
                        w (rg-lin (sin-osc 3) 0.1 0.9)
                        c (rg-lin (sin-osc 8) 0.3 0.8)
                        pulse-freq (rg-lin (sin-osc 2.2) 50 80)
                        trig (lf-pulse 2)
                        env (* (rg-lin (sin-osc 3) 0.8 1)
                               (env-gen (adsr) trig))]
                    (* env (rlpf (clip2 (pulse pulse-freq w) c) freq 0.1)))
                 (transpose [ [400 2000 2550]
                             [620 1200 2400]
                             [300 870 2250]
                             [530 1850 2500]
                             [620 1200 2400]
                             [300 870 2250]
                             [530 1850 2500]]))))

;; 2015.06.18
(defsound test option-d
  (mix (let [p (lf-pulse 1 0 1/8)
                short (lf-pulse 8)
                vol2 (dq short [1 1/2 1/3 1/3])
                base-freq (rg-lin (lf-noise0 1) 200 800)]
            (map (fn [trig snd]
                   (let [env (env-gen (env-perc 0.05 1) trig)
                         freq (* dr (latch:ar (rg-lin (lf-saw 1)
                                                 base-freq (/ base-freq 4))
                                         trig))]
                     (* 3 vol2 env (snd freq))))
                 [(* short p) (* short (- 1 p))]
                 [#(sin-osc (* 2 %)) #(rlpf (saw (* 1/2 %)) %)]))))

;; 2015.06.19 ping
(defsound test option
  (splay (map #(let [noise (- 1 (* 100 (lag-ud (dust 8) 0.01 0.1)))
                        p (lf-pulse 0.1 % 0.7)]
                    (* noise (sin-osc %2) p 4))
                 (range 0 1 0.1)
                 [300 700 600 1200 800 3000 8000 12000])))

(defsound test option
  (splay (map #(let [noise (- 1 (* 100 (lag-ud (dust 8) 0.01 0.1)))
                     p (lf-pulse 0.1 % 0.7)]
                 (* noise (sin-osc (midicps %2)) p 4))
              (range 0 1 0.1)
              [72 70 60 120 80 68 80 90])))

;; 2015.06.20
(defsound test option-d
  (let [p (lf-pulse 1/2 0 0.7)
           freq 50]
    (* (+ (* p (saw (* dr 50)))
          (* (- 1 p) (pulse (* dr 240))))
       (saw (rg-lin (sin-osc 0.1) 95 105)))))

;; 2015.06.20
(defsound test option-d
  (let [f-freq (dt 1 [1 5 3 1/2])]
    (* 4 (rlpf (saw (* dr 50)) (rg-lin (sin-osc f-freq) 20 1000)))))

;; 2015.06.21
(defsound test option-d
  (let [freq (* dr (env-gen (envelope [50 50 100] [2 3])))
        trig (impulse 1/1000 999/1000)
        trig (stepper trig 0 0 1)
        f-env (env-gen (envelope [0 0 5 1000] [5 3 10] EXP))
        p (+ (* trig 1)
             (* (- 1 trig) (lf-pulse f-env)))]
    (* 2 p
       (x-line 0.01 1 5)
       (+ (lpf (saw freq) 2000)
          (sin-osc (* 2 freq))
          (* 1/4 (sin-osc (* 3 freq)))))))

;; 2015.06.21
(defsound test option
  (splay
   (apply map #(let [freq (+ %1 (env-gen (envelope
                                          [0.01 0.01 2000 1 1
                                           500 500 2000 1]
                                          [4 2 1/2 2 1 1 1/2 3] EXP)))
                     t %3
                     w (dt t %2)]
                 (* 8
                    (lf-pulse:ar (/ 1 t) -1/2 w)
                    (+ (sin-osc freq) (saw (* 4 freq)))))
          (transpose
           (repeatedly 10 (fn [] [(+ 200 (rand 1000))
                                  (repeatedly 5 #(rand 1))
                                  (rand 2)]))))))

;; 2015.06.22
(defsound test option
  (let [rythm (env-gen (envelope [4 8 1] [10 4]))]
    (splay
     (map (fn [phase]
            (let [trig (lf-pulse:ar rythm phase 3/4)
                  freq (env-gen (envelope [440 440 800 150 8000] [5 1 4 1]))
                  fenv (env-gen:ar
                        (envelope [0 10 1] [0 (/ 1 5/4 rythm)]) trig)]
              (* 8 trig (sin-osc (* freq fenv)))))
          (range 0 1 0.1)))))

;; 2015.06.23
(defsound test option
  (let [trig (impulse [4 2])]
       (ringz (* (white-noise)
                 (env-gen [ (env-perc 0.1 0.0)
                           (env-perc 0.3 0.0)] trig))
              [ (rg-lin (sin-osc 1) 200 1000)
               (rg-lin (sin-osc 0.4) 500 1000)]
              [0.05 0.1])))

;; 2015.06.24
(defsound test option
  (lpf (ringz (splay (map #(let [trig (lf-pulse 1/10 (+ %1 1/2))
                                 vol (lin-lin %1 0 1/2 1 16)]
                             (* trig (sin-osc %2) vol))
                          (reverse (range 0 1/2 1/40))
                          (range 250 8000 200)))
              (rg-lin (lf-tri 1 1)
                      (x-line 200 2000 10)
                      (line 1800 8000 10)) 0.1)
       2000))

;; 2015.06.25
(defsound test option
  (splay
      (map (fn [phase] (let [env-freq (sum (map #(lf-pulse %) [1 1.8 3.6 12]))
                             env (rg-lin (sin-osc env-freq) 0.1 1)
                             interval (rg-lin (lf-tri 4 phase) 1/2 2)
                             freq-max (dt interval [800 4000 2000 8000])
                             fenv (rg-lin (lf-tri env-freq) 400 freq-max)]
                         (* 4 env (sin-osc fenv))))
           (range 0 2 0.5))))

;; 2015.06.26
(defsound test option
  (splay (map
          #(let [vol-freq (rg-lin (lf-noise0 1) 1/8 1)
                 vol (cubed (lin-lin (abs (sin-osc vol-freq)) 0 1 0.3 1))
                 rythm (rg-lin (lf-noise0 1) 50 300)
                 fenv (lin-exp (demand (impulse:ar rythm) 0 (dbrown 0 1 0.01))
                               0 1
                               800 4000)
                 trig (step % 0.3)]
             (* 16 vol (saw fenv) trig))
          (flatten [ [0 5 6 7 8 9]
                    (repeat 5 14)]))))

;; 2015.06.26
(defsound test option-d
  (let [vib (rg-lin (sin-osc 2) 0 20)
           snd (mix (map #(sin-osc (+ % vib))
                         (take 10 (iterate #(+ (rand 200) 200 %) 100))))
           freq (rg-lin (lf-tri (dt 2 [0.1 0.3 0.5 1 1 3])) 400 2000)]
    (ringz snd (* dr freq) (line 0.3 0.03 8))))

;; 2015.06.27
(defsound test option-d
  (let [freq-min (rg-lin (lf-noise1 8) 600 1500)
        freq-max (rg-lin (lf-noise1 8) 1500 2500)]
    (* 8 (resonz (pulse (* dr (dt 2 [400 700 200 250]))
                        (rg-lin (sin-osc (line 1/4 10 10)) 0.1 0.9))
                 (rg-lin (lf-tri 2.2) freq-min freq-max) 0.1))))

;; 2015.06.27 (ugomeki)
(defsound test option
  (let [snd (sum (map #(* (/ 1/2 %1) (sin-osc %2))
                        (iterate inc 1)
                        (take 10 (iterate #(* % (+ 1 (rand 1))) 100))))]
      (splay (map #(ringz snd % 0.05)
                  (map #(rg-lin (sin-osc % (/ Math/PI %))
                                (* 100 %)
                                (* 300 %))
                       (range 1 5))))))

;; 2015.06.29 kirakira
(defsound test option
  (let [base-freq (rg-lin (lf-noise0 4) 3000 5000)
           trig (impulse 1/4)
           env #(env-gen:ar (env-perc 0.05 (/ 4 %)) trig)
           snd (map #(* (sin-osc %) (env %2))
                    (take 20 (iterate #(* (+ 1 (rand 0.1)) %) base-freq))
                    (iterate inc 1))]
       (* 20 (splay snd))))

;; 2015.06.29
(defsound test option-d
  (let [trig (lf-pulse:ar 1 (sin-osc 4))]
       (switch trig (* (lf-noise0 (x-line 4 100 20)) (white-noise))
               (* 20
                  (mix (map #(* (sin-osc %)
                                (env-gen:ar (env-perc 0 0.01)
                                            (dust 4)))
                            (range 200 2000 200)))))))

;; 2015.06.30
(defsound test option-d
  (let [trig0 (lf-pulse 1/4 1/2)
        trig (lf-pulse (env-gen (envelope [2 2 4 4 8] [15 0 5 10])))
        freq (* dr (lin-lin (demand trig 0 (dbrown 0 1 0.05)) 0 1 300 1500))
        env (env-gen (envelope [0 1 1 0] [0.05 0.4 0.1]) trig)
        freq (lag-ud freq 0.05 0.03)
        trig2 (impulse (env-gen (envelope [8 8 12 12 16] [15 0 5 10])))
        freq2 (demand trig2 0 (dbrown 0.6 1.4 0.05))
        env2 (env-gen (envelope [0 1 1 0] [0 (* 7/8 1/8) (* 1/8 1/8)])
                      trig2)]
    (+ (* trig0
          (mix (map #(* 4 (pulse (* freq %) 0.3) env %2)
                    [1 3/2 8/3 1/2]
                    [1 0.8 0.3 0.4])))
       (* (- 1 trig0)
          (pulse (* freq freq2) 0.4)
          env2))))

;; 2015.06.30
(defsound test option-d
  (+ (sin-osc (* dr (dt 2 [200 400 800 2000 4000])
                 (vib 1/50 (dt 10 [3 8]))))
     (* 1/4 (sin-osc (* (dt 4 [800 2000 4000])
                        (vib 1/10 (dt 10 [1/10 1/8])))))))

;; 2015.07.01
(defsound test option-d
  (let [rythm (flatten [[2 2 4 4 2 2 4 4]
                           [2 2 8 8 8 8 4 4]
                           [2 2 2 2 8 8 4 4]])
        freq (* dr (eg-seq [2500 1000 800 400 300] 4 (impulse 1/20)))]
       (* (lag (lf-pulse:ar (dt 1/4 rythm) 0.8) 0.01)
          (pulse (* (vib 1/20 0.1) freq))
          (+ (* (step 10 10) (sin-osc (* 5/4 freq)))
             (- 1 (step 10 3)))
          (+ (* (step 20 5) (sin-osc (* 2/3 freq)))
             (- 1 (step 20 3))))))

;; 2015.07.03
(defsound test option-d
  (* 12
     (vib 1/10 2)
     (sin-osc (* dr 440))
     (sin-osc (rg-lin (lf-saw (x-line 0.01 3000 30))
                      30 (env-gen (envelope [300 300 3000] [25 5]))))))

;; 2015.07.04
(defsound test option
  (splay (apply map (fn [freq t phase]
                      (let [trig (impulse (/ 1 (* 2 t)) phase)]
                        (* 5
                           (vib 1/5)
                           (sin-osc (* (vib 1/50) (env-gen (envelope [0 freq freq (/ freq 2)] [0 t 1]) trig)))
                           (env-gen (envelope [0 1 1 0] [5 (- t 5) 1]) trig))))
                (transpose (repeatedly 10 (fn [] [(exp-rand 200 3000)
                                                  (+ 5 (rand 10))
                                                  (rand 1)]))))))

;; 2015.07.04
(defsound test option-d
  (* (lf-pulse:ar (dt 1 [32 6 12 64]))
        (lf-pulse:ar 1)
        (saw (* dr (dt 2 [200 150 400 370])))))

;; 2015.07.05
(defsound test option-d
  (let [freq (* dr (dt 1/2 (flatten [[200 200 0 200 200 0 250]
                                     [200 200 0 300 300 0 150]])))
        rythm (lf-pulse 2 0.9)]
    (* rythm
       (hpf (rlpf (+ (* 1/2 (pulse (* 2 freq (vib 1 (/ freq 4)))))
                     (pulse (/ freq 2)))
                  (* freq (lin-lin (env-gen (envelope [1 0 1] [0.05 0.2]) rythm) 0 1 1 10)))
            100))))

;; 2015.07.06
(defsound test option
  (let [trig (lf-pulse [1/2 2] [0 1/3] 0.4)
           freq [(dt 4 [500 450 800 700])
                 (dt 2 [200 150 200 100])]
           gap (line 1.005 1.05 16)]
       (* (rlpf (+ (pulse freq (rg-lin (sin-osc 1.4) 0.3 0.7))
                   (pulse (* gap freq) (rg-lin (sin-osc 3) 0.2 0.8)))
                1000 0.2)
          (env-gen (adsr 0.01 0.2 1 0.5) trig))))

;; 2015.07.06
(defsound test option-d
  (let [freq (* dr 500)]
    (map (fn [freq] (let [trig (lf-pulse 1/2 0 0.5)
                          chorus-vol (dt 2 [0.02 0.05 0.1 0.5 0.8])
                          chorus (rg-lin (lf-noise2 [0.4, 0.5, 0.7, 1, 2, 5, 10]) 1 (+ 1 chorus-vol))
                          fenv (* freq (+ 2 (env-gen (envelope [0.03 2 0.01] [0.2 0.2] EXP) trig)))
                          env (env-gen (adsr 0 0.0 1 0.5) trig)]
                      (mix (* 4 env (lpf (saw (* chorus freq)) fenv)))))
         [ (cents freq 3.2)
          (cents freq -3.2)])))

;; 2015.07.07
(defsound test option-d
  (let [freq (* dr (env-gen (envelope [400 400 1000 3000 400 200] [8 4 4 1 3])))
        env (lag-ud (lf-pulse 1/4 0 0.9) 0.5 0.3)
        pwm (switch (lf-pulse 1)
                    (lf-saw 1/4 1)
                    (lf-saw 4 1))
        w (lin-lin pwm -1 1 0.05 0.95)]
    (* env (pulse freq w))))

;; 2015.07.08
(defsound test option-d
  (let [trig (lf-pulse:ar (dt 4 [2 4]) 0 0.6)
        freq (* dr (dt 4 [200 150 308 270]))
        env (env-gen:ar (env-adsr 0 0 1 0) trig)]
    (* (free-verb (bpf (+ (pulse (/ (+ freq (* 20 (sin-osc 1/4))) 2))
                          (* 1/2 (sin-osc freq)))
                       (* (vib (x-line 1 16 16) 16) freq (env-gen:ar (envelope [1 2 1] [0 0.05]) trig)))
                  1 3)
       env)))

;; 2015.07.09
(defsound test option
  (let [freq (rg-lin (sin-osc [0.1 0.15] [0 1]) 400 600)
           r (demand (impulse 8) 0 (dibrown 6 16 1))
           trig (impulse r)]
       (* (+ (* (vib 1/10 2) (sin-osc freq))
             (pulse (/ freq 2) (rg-lin (sin-osc 1.5) 0.1 0.9)))
          (env-gen (env-perc 0.01 (/ 1 r)) trig))))

;; 2015.07.09
(defsound test option-d
  (let [param [(m-map lf-pulse [1 7 19 11])
                  (latch:ar (abs (sin-osc (line 1 10))) (impulse 16.01))]]
    (saw (* dr (lin-exp param 0 1 200 3000)))))

;; 2015.07.10
(defsound test option-d
  (let [param [(m-map lf-saw [1/10 1/3 12])
                  (m-map sin-osc [9 7])]]
    (pulse (* dr (lin-exp param -1 1 300 3000)))))


;; 2015.07.11
(defsound test option-d
  (let [param (m-map (fn [freq]
                       [ (lf-pulse:ar freq)
                        (lf-pulse:ar (/ freq 4) 0.1)])
                     [(dt 4 [1     2   9])
                      (dt 2 [4 4.5 3 8 4 12])
                      (dt 4 [2     2.1 7])])
        param (lag-ud param 0.001)]
    (* 1/2 (sin-osc-fb (* dr (lin-exp param 0 1 300 3000))
                       (rg-lin (sin-osc 1.05) 0 1)))))


;; 2015.07.12
(defsound test option-d
  (let [freq 800
        snd (pulse freq)
        mixed (m-map lf-saw [(dt 6 [1 12 18])
                             (dt 6 [8 60 38])
                             (dt 6 [3 1/3 300])])
        sweep (lin-exp mixed -1 1 100 2000)]
    (moog-ff snd (* dr sweep) 3.8)))

;; 2015.07.12
(defsound test option-d
  (let [freq (rg-lin (lf-noise0 1) 100 800)
        snd (m-map #(saw (* % freq)) [1 3/2 5/4])
        x (m-map #(clip:ar (f-sin-osc %) 0 0.1) [1/3 1/12 1/7 1/13])
        sweep (lin-exp x 0 0.1 100 2000)]
    (lpf (ringz snd (* dr sweep) 0.8) 2000)))

;; 2015.07.13
(defsound test option
  (let [snd (pm-osc (rg-lin (sin-osc 8) 280 800)
                      (rg-lin (sin-osc 5.3) 280 800) 10)
          drum (fn [freq] (* (env-gen (env-perc 0.01 0.05) (lf-pulse freq)) snd))]
      (* 2 (map #(m-map drum %) [[1 7 1/3 9] [2/3 1.2 1/3]]))))



;; 2015.07.14 (like)
(defsound test option
  (splay (repeatedly 10
                       #(let [freq (rg-exp (lf-noise0:kr 1) 500 4000)
                              delay (rg-lin (lf-saw:kr -1 1) 0 0.05)]
                          (ringz (pulse (/ freq 4) (rg-lin (lf-noise0 32) 0 1)) freq delay)))))

;; 2015.07.14
(defsound test option
  (splay (map (fn [phase]
                   (let [freq (latch:ar (m-map #(lf-pulse % phase) [1 1/3 31 32]) (impulse 16.05))
                         snd (saw (rg-lin (lf-tri 1/10) 10 80))]
                     (* 8 (rlpf snd (lin-exp freq 0 1 300 3000) 0.1))))
                 (range 0 1 0.1))))

;; 2015.07.15
(defsound test option-d
  (let [trig (lag (m-map lf-pulse [0.9 3.1 1/4]) 0.01)]
       (switch trig
               (rlpf (* (env-gen (env-perc 0.05 0) (impulse 32)) (white-noise))
                     (* dr (rg-lin (lf-noise0 4) 200 4000)))
               (* 1/2 (lf-pulse 12 0 1/4) (white-noise)))))

;; 2015.07.15
(defsound test option-d
  (let [sw (lf-pulse:ar (rg-lin (lf-saw [4 1/2]) 2 8))
        freq (* dr (lin-exp (m-map #(lf-pulse:kr % (rand 1)) [4 1 3.8]) 0 1 200 2000))
        freq2 (x-line 1000 800 10)]
    (switch sw
            (+ (sin-osc (/ freq 3)) (lf-tri freq))
            (pm-osc freq2 (rg-exp (lf-noise0:kr 4) 600 800) 8))))

;; 2015.07.17
(defsound test option
  (splay (map (fn [phase] (let [trig (impulse 1/4 phase)
                                   freq (demand trig 0 (drand (range 500 2000 200) INF))
                                   env (env-gen (envelope [0 1   1    0 0 1 0.8 0]
                                                          [0 0.1 0.05 0.1 0.05 0.1 3]) trig)]
                               (* 16 (m-map #(sin-osc (* freq %)) [1 3/2 4/5]) env)))
                 (range 0 1 0.1))))

;; 2015.07.17
(defsound test option
  (let [freq (rg-lin (sin-osc 1/12) 500 530)
           sq (lag-ud (lf-pulse 8) 0.08 0)
           ratio (map #(demand (impulse 1/4 %) 0 (drand [1 11/5 3/2 4/5 2 1/2 3/4 4/3] INF)) (range 0 1 1/6))
           snd (m-map #(lf-tri (* freq %)) ratio)]
       (* sq snd 2)))


;; marinba
(defsound test option
  (let [amp 0.3, sustain 0.5, freq 400, rq 0.006
        gate (lf-pulse 2)
        env (env-gen (adsr 0.0001 sustain (/ sustain 2) 0.3) gate)
        b1 (* 1.987 0.9889999999 (cos 0.09))
        b2 -0.998057
        signal (sos (lf-pulse:ar 1 0 1) 0.3 0.0 0.0 b1 b2)
        signal (+ (rhpf (* 0.8 signal) freq rq)
                  (delay-c (rhpf (* 0.9 signal) (* freq 0.99999) (* rq 0.999))
                           0.02 0.01223))
        signal (* signal (decay2 signal 0.4 0.3))]
      (* signal env amp 0.65)))

;; wood
(defsound test option
  (let [amp 0.3, sustain 0.5, freq 400, rq 0.06
          env (env-gen (env-perc 0.00001 sustain) :action FREE)
          b1   (* 2.0 0.97576  (cos 0.161447))
          b2   (* -1 0.9757 0.9757)
          signal (sos (lf-pulse:ar 1 0 1/20) 1.0 0.0 0.0 b1 b2)
          signal (* signal (decay2 signal 0.4 0.8))
          signal (limiter (resonz signal freq (* rq 0.5))
                          0.9)]
      (* env signal amp 10)))

;; 2015.07.17
(defsound test option
  (let [trig (impulse 4)
           freq (lag (demand trig 0 (dbrown 400 1200 100)) 0.6)
           ratio (env-gen (envelope [1 8 0.5 8] [0 0.1 0.1]) trig)
           snd (rlpf (saw freq) (* freq ratio))]
       snd))

;; 2015.07.17
(defsound test option
  (let [freq (rg-exp (lf-noise1:kr 2) 300 800)
           snd (pulse freq )
           rq 1
           snd (+ (rhpf (* 0.8 snd) freq rq)
                  (delay-c (rhpf (* 0.5 snd) (* freq 0.99999) (* rq 0.999))
                           0.2 0.1223)
                  (delay-c (rhpf (* 0.3 snd) (* freq 0.6) (* rq 0.4))
                           0.4 0.3223))]
       (* (rg-lin (sin-osc (line 1/2 8 10)) 0.2 1) snd)))

;; 2015.07.18
(defsound test option
  (let [freq [100 50]
           phase [0 0.5]]
       (limiter (* (rhpf (saw freq)
                         (* freq
                            (lin-lin (m-map #(sin-osc % phase) [2 1/3 1/12]) -1 1 1 50))
                         (rg-exp (sin-osc:kr 1 (rand 1)) 0.0001 1))
                   (lag-ud (lf-pulse 1/4 phase 0.75) 0.5 0.3)))))

;; 2015.07.19
(defsound test option
  (let [freq (rg-lin (lf-noise0 32) 450 500)
           f-env (pulse (lin-lin (switch (lf-pulse 1/2)
                                         (lf-noise0 4)
                                         (lf-noise1 4))
                                 -1 1 100 800))
           snd (sin-osc (* freq f-env))]
       snd))


;; 2015.07.19
(defsound test option-d
  (let [freq (rg-exp (lf-noise1:kr 2) 100 3000)
           snd (rlpf (m-map #(saw (* % freq)) [1 3/2 4/5 1/4])
                     (* dr freq (lin-lin (switch (lf-tri 1/2)
                                              (white-noise)
                                              (sin-osc 8)) -1 1 1 (line 1 5 10)))
                     (rg-exp (sin-osc:kr (rg-exp (sin-osc:kr 2) 0.01 20)) 0.001 0.1))]
       (* (cubed (vib 2/3 1/2)) snd 2)))

;; 2015.07.21
(defsound test option
  (let [freq (dt [1/3 1/3 1/4 1/4 1/3 1/3 2/3] [800 450 430 420 620])
           pulse-freq (dt [1 1/3 1/3 2/3] [4 10 8 2])
           pulse-width (dt [1/4 1/3 4/3] [1/2 1/16 1/18])
           env #(lf-pulse:ar pulse-freq % pulse-width)]
       (splay (* 10 (map env (range 0 1 1/5)) (sin-osc freq)))))

;; 2015.07.21
(defsound test option-d
  (* (ringz (clip2 (rlpf (white-noise) (* dr (rg-exp (lf-saw:kr 2.2) 100 3000)))
                      (rg-exp (lf-noise1:kr 100) 0.1 1))
               (rg-exp (lf-tri:kr (line:kr -4 4 10)) 100 3000)
               0.5) ))
;; 2015.07.21
(defsound test option-d
  (let [freq (* dr (rg-exp (lf-pulse:kr 100)
                      (dt:kr 1 [1000 1500])
                      (dt:kr 1/2 [6000 2000 2500 4300])))]
       (* 3/2
          (sin-osc freq)
          (mk-rythm 0.1 [1/4 1/4 1/8 1/8 1 1/3 1/3 1/16 1]))))

;; 2015.07.22
(defsound test option
  (let [freqs (iterate #(+ % (rg-lin (lf-noise2:kr 2.5) 100 300)) 50)
        snd (map #(* (sin-osc (* % (lin-lin (abs (clip:ar (sin-osc 1/6 %2)
                                                            -0.3 0.5))
                                              0 0.5
                                              0.5 5/2))))
                   freqs
                   (range 0 1 1/20))]
    (splay (ringz snd (rg-exp (lf-tri:kr 5) 100 6000) 0.008))))

;; 2015.07.23(need memory)
(defsound test option
  (splay (map (fn [fr phase]
                   (let [trig (impulse 1/8 phase)
                         freq (lin-lin (env-gen (envelope [0 0 -0.2 1 1 0]
                                                          [1 1/2 1 3 1/8] EXP) trig) -0.2 1
                                       fr (* fr 4))
                         vol 1/40
                         vib (lin-lin (sin-osc 1/2) -1 1 (- 1 vol) (+ 1 vol))
                         env (rg-lin (sin-osc (lf-noise0 4)) 0.1 1)]
                     (* 2 env (m-map #(sin-osc (* vib % freq)) [1 5/3 4/5]))))
                 (iterate #(+ % (rand 800) 100) 100)
                 (repeatedly 10 #(demand (impulse 1) 0 (dbrown 0 1 0.1))))))
;; 2015.07.25
(defsound test option
  (let [trig (impulse 1/3)
           freqs [(lin-lin (env-gen (envelope [0 1 1.2 1.8 0 2 0 2]
                                              [0.1 1 0.2 0.5 0.1 0.1 0.1]) trig)
                           0 2 200 350)
                  (lin-lin (env-gen (envelope [0 1 2 0 1 0 1.5]
                                              [0.4 0.25 0.05 0.2 0.2 0.1]) trig)
                           0 2 50 320)]]
       (map (fn [freq alpha] (m-map (fn [index]
                                      (let [env (m-map #(lf-saw % (/ 1 index)) [1/10 2.2 alpha])
                                            snd (sin-osc (* index freq))]
                                        (* env snd 8)))
                                    (range 1 20 2)))
            freqs [3 0.8])))

;; 2015.07.27
(defsound test option
  (let [trig (impulse [0.8 (env-gen (envelope [0 7 0] [6 4]))])
           freq (dq trig [500 360 450 800 300])
           base (pulse freq 1/8)
           sub (* (rg-lin (sin-osc 2.11) 0 1) (lf-pulse:ar freq 1/8 1/8))
           env (env-gen (adsr 0.05 0.05 0.8) trig)
           snd (softclip (* env (+ base sub)))
           snd-d (take 8 (iterate #(+ % (* 0.6 (->> (delay-c % 0.5 (ranged-rand 0.05 0.5))
                                                    (distort) (tanh)))) snd))]
       (+ snd snd-d)))

;; 2015.07.27
(defsound test option
  (let [freq 400
           d (dt 1/2 [8 32 64 2])
           mod-freq (+ (/ freq 2) d)
           snd (* (lf-pulse 16)
                  (pm-osc freq mod-freq (rg-lin (sin-osc 1) 4 20)))]
       (switch (lf-pulse 1/4)
               snd
               (distort (fold:ar (softclip (tanh snd)) -0.4 0.6)))))

;; 2015.07.29
(defsound test option
  (let [freq (rg-exp (lf-noise0:kr 8) 100 10000)
           snd (* (sin-osc freq)
                  (fold:ar (lf-tri (rg-exp (sin-osc:kr 2.5) 1 100))
                           (rg-lin (sin-osc 4) 0 1)))]
       (limiter (* 12 (ringz (- snd (bpf snd freq 1))
                             freq 0.01)))))

;; 2015.07.30
(defsound test option
  (splay
     (map #(let [trig (dust:kr 1)]
             (* 16
                (env-gen (env-perc %1 %2) trig)
                (rlpf (choose [(saw)
                               (pulse)])
                      (lin-lin (env-gen (env-perc %3 %4) trig)
                               0 1
                               3000 400) %5)
                (lf-pulse %6 (rand 1))))
          (repeatedly 10 #(ranged-rand 0.05 0.5))
          (repeatedly 10 #(ranged-rand 0.05 0.5))
          (repeatedly 10 #(ranged-rand 0.05 0.5))
          (repeatedly 10 #(ranged-rand 0.05 0.5))
          (repeatedly 10 #(ranged-rand 0.05 0.5))
          (repeatedly 10 #(choose [8 16 1])))))
;; 2015.07.31
(defsound test option
  (splay (apply map #(let [trig (lf-pulse (a2k (dt 2 [1 2 4 8 16])))
                              env (env-gen (adsr %2 %3 %4 %5) trig)
                              fenv (lin-lin (env-gen (adsr %6 %7 %8 %9) trig) 0 1 400 3000)]
                          (* 16 (clip:ar (sin-osc 1/8 %1) 0 0.5) env (rlpf (choose [(sin-osc)
                                                                                    (saw)
                                                                                    (pulse)]) fenv)))
                   (flatten [ (range 0 (* 2 Math/PI) (* 1/5 (* 2 Math/PI)))
                             (range 0.05 (* 2 Math/PI) (* 1/5 (* 2 Math/PI)))])
                   (repeatedly 8 #(repeatedly 10 (fn [] (ranged-rand 0.01 0.2)))))))

;; 2015.08.01
(defsound test option
  (let [snd (map #(rlpf (white-noise) (lin-lin %1 -1 1 200 4000) %2)
                    [(sin-osc 1)
                     (lf-saw 2)
                     (cubed (lf-tri 4/3))
                     (mix (lf-pulse [1 2/3 4]))]
                    [0.3
                     (rg-exp (sin-osc:kr 1) 1 0.01)
                     0.3
                     (rg-exp (lf-saw:kr 2) 1 0.01)])]
       (splay (switch (lin-lin (clip:ar (lf-tri 1/8) 0 0.5) 0 0.5 0 1)
                      snd (g-verb snd)))))

;; 2015.08.01
(defsound test option
  (let [freq (rg-lin (lf-noise1 (dt 3 [1 2 8 16])) 200 1000)]
       (m-map #(* (delay-n (sin-osc freq) 0.5 %)
                  (* 2 (- 0.6 %))) (range 0 0.5 0.1))))

;; 2015.08.02 (like) rythm
(defsound test option
  (splay (leak-dc (map #(ringz (* 4 %1 (white-noise)) %3 %2)
                          [(impulse 2)
                           (impulse (rg-exp (lf-saw:kr 1/8) 1/10 8) 0.1)
                           (impulse (rg-exp (lf-saw:kr 1/4) 1/10 8))
                           (impulse (rg-lin (lf-pulse:kr 1/6 0 7/8) 16 4))
                           (impulse 1/8)]
                          [0.5 0.1 0.2 0.01 1]
                          [300 560 1200 2400 600]))))

;; 2015.08.03
(defsound test option-d
  (let [freq 500
        lfo (* dr (rg-lin (lf-tri 12) 0 0.5))
        snd1 (m-map sin-osc [freq (* freq 4/5 1/2) (* freq 2/3 1/4)])
        snd2 (rlpf (pulse (* (+ 1 lfo) freq) 1/2) 12000)]
    (switch (lf-pulse:ar (dt 4 [1/4 1/4 2 8 8.1])) snd1 snd2)))

;; 2015.08.05
(defsound test option-d
  (let [trig (lf-pulse (a2k (dt 1 [1/2 4 1/2 2 2])))
        freq (* dr (dq trig [500 450 800 700 720 750]))
        snd (+ (m-map #(saw (add-cents freq %)) [5 10 -5])
               (lf-tri (* 2 freq (rg-lin (sin-osc 3) 0.99 1.01))))
        fenv (lin-lin (env-gen (adsr :release 0.1) trig) 0 1 1000 10000)
        snd (rlpf snd fenv 0.5)
        env (env-gen (adsr) trig)]
    (* env snd)))

;; 2015.08.06
(defsound test option
  (splay (map #(let [trig (env-gen (envelope [0 0 1 1 0] [%1 1/2 (- 14 %1) 3]))
                        env (lin-lin trig 0 1 1 3)
                        fenv (lin-lin trig 0 1 400 %2)]
                    (* env (sin-osc fenv)))
                 [1.5 2.5 3.5 4 5 7 8.5 9.5 10 11.5 12 12.5 12.5 12.5 12.5]
                 [500 700 817.974 1200 1354.874 1539.773 1881.984 2076.3 3070.793 4622.753 5811.519])))

;; 2015.08.07
(defsound test option
  (* (hpf (white-noise) (rg-exp (lf-saw:kr (dt:kr 1 [8 2 8 4 32 32]) 1) 1000 10000))
        (lf-pulse:ar (dt 1 [2 2 8 16]))))

;; 2015.08.07
(defsound test option
  (* (sin-osc (rg-lin (lf-saw 1/10 -1) 10 200))
        (switch (lf-pulse 1/2)
                (lpf (white-noise) 5000)
                (hpf (white-noise) 5000))))

;; 2015.08.10
(defsound test option
  (let [trig (* (lf-pulse 1)
                   (lf-pulse (a2k (dt 2 [4 4 8 4 8 4 8]))))
           arr [300 382 492 562 639 622 681 588 690 646 917 458.5]
           freq [(dq (t-delay trig 0.05) (map #(* % 2/3) arr))
                 (dq trig arr)]]
       (free-verb (* (env-gen:ar (env-perc 0.01 0.5) trig)
                     (sin-osc freq))
                  0.5 (line 0.1 4 16))))


;; 2015.08.10
(defsound test option
  (splay (map #(* (sin-osc %2)
            (env-gen (envelope [0 1 1 0] [0 4 1]) (step %)))
        (iterate #(+ % 1/5) 0)
        [300 333.985 484.427 614.667 386.595
         353.188 219.915 449.967 597.969 1359.628
         710.73 442.02])))

;; 2015.08.11
(defsound test option
  (splay (repeatedly 8 #(let [freq (demand (impulse 32) 0 (dbrown 400 600 30))]
                    (sin-osc [(* (rg-exp (lf-saw:kr 1/10 1) 1 8) freq)
                              (* (rg-exp (lf-tri:kr 1/4 1) 3/4 4/3) freq)])))))

;; 2015.08.12
(defsound test option
  (splay (map #(let [freq %1
                        trig (impulse:kr 1/5)
                        snd (mix [ (pulse freq (rg-lin (sin-osc 3) 0.1 0.5))
                                  (sin-osc freq)
                                  (lf-tri (* 1/4 freq))])
                        env (env-gen (envelope [0 0.5 1 1 0] [0.2 0.2 0.4 0.8]) trig)
                        env (+ env (* (sin-osc 5) (env-gen (env-perc 0.8 2) (t-delay trig 0.8))))]
                    (* env snd))
                 [(dt 5 [  500      2000      2500      600])
                  (dt 5/2 [800 3000 1100 5000 400 2000])
                  (dt 5 [ 1200      1800      550       900])])))

;; 2015.08.13
(defsound test option
  (splay (map #(let [trig (impulse 3/2 %2)
                        freq %]
                    (* (rlpf (pulse freq (env-gen (envelope [0 0.2 0.01] [0 0.5]) trig))
                             (* (env-gen (envelope [0 4 1 10] [0 0.3 0.05]) trig) freq) 1.5)
                       (env-gen (envelope [0 1 1 0] [0.01 0.5 0.01]) trig)))
                 [(dt 1   [400 800])
                  (dt 3   [600 700 800])
                  (dt 1/2 [200 400 300 400 100 1000 800 600])]
                 [0 0 0.5])))

;; 2015.08.14
(defsound test option
  (let [trig (a2k (lf-pulse:ar (dt 1 [1 1 4]) 0 (dt 1 [1/2 1/8 1/8])))
           freq [ (dt 1 [400 550 800 200])
                 (dt 2 [400 550 800 200])]]
       (+ (* (env-gen (adsr 0.2 0.1 1 0.5) trig) (sin-osc freq))
          (* 1/2 (env-gen (adsr 0.01 0.01 1 0.3) trig)
             (rlpf (pulse (* 3 freq))
                   (* (lin-lin (env-gen (env-perc 0.0 0.1) trig) 0 1 2 20) freq))))))

;; 2015.08.15
(defsound test option
  (map (fn [freq1 freq2]
            (m-map #(* (bpf (saw freq1) (rg-exp (sin-osc:kr 1/5 %) 1000 8000) 0.1)
                       (rg-lin (sin-osc freq2) 2 8))
                   (range 0 3 1)))
          [200 350]
          [12 1/10]))

;; 2015.08.16
(defsound test option-d
  (let [trig (impulse 1)
        freq (* dr (midicps (dq trig [70 73 75 75 78 82 85 85])))
        snd (* (sin-osc (rg-lin (lf-pulse 8) 300 freq)) (env-gen (env-perc 0.5 0) trig))]
    (comb-n snd 1/4 1/4 1)))


;; 2015.08.16
(defsound test option
  (let [trig (impulse 8)
          freq (dq trig (flatten [ (range 400 1200 100)
                                  (range 2000 2200 50)]))
          env (env-gen (envelope [0,1,0.5,0.0] [0.02,0.1,0.1]) trig)
          filter (* 0.3 (b-low-pass-4 (white-noise) freq 0.01))]
      (comb-l (pan2 (* env (+ (* 0.7 filter) (* 0.3 (distort filter)))))
              1/4 1/4 1)))

;; 2015.08.17 (like) go-kaku
(defsound test option
  (let [trig (impulse 8)
          freq (dq trig (flatten [ (range 400 1200 100)
                                  (range 2000 2200 50)]))
          env (env-gen (envelope [0,1,0.5,0.0] [0.02,0.1,0.1]) trig)
          filter (* 0.3 (b-low-pass-4 (pulse freq 0.01) freq 0.01))]
      (comb-l (pan2 (* env (switch (line 0 1 8) filter (distort filter))))
              1/8 1/8 1.2)))

;; 2015.08.19
(defsound test option
  (let [trig (impulse 1 [0 0.01])
          freq (dq trig [500 418.603 562.246 695.444 875.04 1008.593 1104.681])
          fenv (env-gen (envelope [0 4 0 1 1/4 3] [0.01 0.2 0.01 1/2 1/2]) trig)]
      (rlpf (lf-tri (* freq fenv))
            (rg-lin (lf-saw 12) (* 2 freq) (* freq 8)))))

;; 2015.08.21 (like) melody?
(defsound test option
  (let [freq [(dt [4 1/2 1/2] [1200 2036 1824 1556 1440 1268 1112])
                 (dt [1/2 1/2 2] [600 948 450 773 600 948 1138])]
           snd (* [2 1]
                  (sin-osc 2 [0 1])
                  (sin-osc 3 [0 1/2])
                  (- 1 (* (sin-osc 1/8) (sin-osc 80)))
                  (- 1 (* (step 5 7) (sin-osc 300)))
                  (rg-lin (sin-osc 1/3) 1/3 1)
                  (sin-osc freq))]
       (switch 0.8 snd (delay-n (comb-l snd 1/4 1/4 1/2) 1 (rg-lin (sin-osc 1/4) 0.1 1)))))

;; 2015.08.21
(defsound test option
  (let [freq [(dt 1/8 [430 580 540])
              (dt 1/4 [500 800 300 380])]
        fenv (rg-lin (lf-tri 1/2) 3 8)]
    (mix (* (rg-lin (lf-pulse (a2k (dt 2 [2.8 5]))) 0.6 1)
            (- 1 (rg-lin (sin-osc 3) 0 (line 0 1 10)))
            (lpf (lf-tri freq) (* freq fenv))))))

;; 2015.08.22
(defsound test option-d
  (* (lf-pulse (a2k (dt 4 [3 8 12])) 0
               (a2k (rg-lin (lf-tri
                             (a2k (dt 1 [4.5 8.1 32]))) 0.1 0.9)))
     (saw (* dr 600))))

;; 2015.08.30 (like) happy
(defsound test option-d
  (let [trig (impulse (dt:kr 2 [4 12 32]))
        freq (* dr (dq trig (range 1000 10000 300)))
        snd (* (sin-osc (rg-lin (lf-pulse 8) (max 300 (/ freq 4)) freq))
               (env-gen (env-perc 0.5 0) trig))]
    (comb-n snd 1/4 1/4 1)))

;; 2015.08.30 (like) happy
(defsound test option-d
  (let [trig (impulse 12)
        freq (latch:ar (rg-lin (lf-tri 1/4 -1) 500 8000) trig)
        env (env-gen (envelope [0,1,0.5,0.0] [0.02,0.1,0.1]) trig)
        filter (* 0.3 (b-low-pass-4 (pulse freq 0.01) freq 0.01))]
    (comb-l (* env (switch (rg-lin (lf-tri 1/32) 0 1) filter (distort filter)))
            1/8 1/8 1.2)))

;; 2015.09.03 (why?)
(defsound test option
  (* (sin-osc (rg-lin (lf-pulse (line:kr 4 8 10))
                         400
                         (+ (sin-osc 1) 800)))
        (lf-pulse:ar (dt 1 [3 1.3])
                     (rg-lin (sin-osc 1) 0.1 0.9))))

;; 2015.09.05
(defsound test option-d
  (let [trig (impulse 1)]
       (* 4 (lpf (m-map sin-osc
                        (map #(* 400 %) (take 5 (iterate inc 1))))
                 (* dr (env-gen (envelope [0 4000 100 1000] [0 0.3 0.1]) trig)))
          (env-gen (env-perc 0.05 0.7) trig))))

(defsound test option-d
  (let [trig (impulse 1)]
       (* 4 (lpf (m-map sin-osc
                        (->> [1 1.078 1.191 1.381 1.879 1.919 2.43 3.424]
                             (map #(* 400 % %2) (iterate inc 1))))
                 (* dr (env-gen (envelope [0 4000 100 1000] [0 0.3 0.1]) trig)))
          (env-gen (env-perc 0.05 0.7) trig))))

;; 2015.09.05
(defsound test option-d
  (let [trig (impulse 1)
        fenv (env-gen (perc 0.01 0.03) trig)]
    (* 4 (lpf (m-map sin-osc
                     (->> [1 1.078 1.191 1.381 1.879 1.919 2.43 3.424]
                          (map #(* 400 % (+ 1 (* fenv %2)))
                               (iterate inc 1))))
              (* dr (env-gen (envelope [0 4000 100 1000] [0 0.3 0.1]) trig)))
       (env-gen (env-perc 0.05 0.7) trig))))

;; 2015.09.05
(defsound test option
  (splay (let [trig (impulse 1)]
     (* (map #(sin-osc (* % (+ 1 (* %4 (env-gen (env-perc %2 %3) trig)))))
               (iterate #(+ 500 %) 500)
               (repeatedly 10 #(rand 0.1))
               (repeatedly 10 #(rand 0.1))
               (repeatedly 10 #(rand 3)))
        (env-gen (env-perc 0.05 0.5) trig)))))

;; 2015.09.06 (like) from space
(defsound test option
  (let [trig (impulse 1/10)
           whole (lin-lin (env-gen (env-perc 0 10) trig) 0 1 1000 10000)]
       (* (mix (repeatedly 5 #(sin-osc
                               (* whole (+ (line 2 0.1 10)
                                           (lf-noise0 8))))))
          (sin-osc [3.28 1.34]))))

;; 2015.09.07
(defsound test option
  (let [trig (impulse 1/10)
        whole (lin-exp (env-gen (env-perc 0 10) trig) 0 1 1000 15000)]
    (splay (repeatedly 12
                 #(let [phase (demand trig 0 (drand (range 0 1 1/20)))
                        trig2 (impulse 6 phase)]
                    (* 3
                       (sin-osc
                        (* whole (+ 1 (demand trig2 0
                                              (drand (range 0 1 1/20))))))
                       (env-gen (env-perc 0.05 0.1) trig2)))))))

;; 2015.09.07
(defsound test option
  (let [z (* (white-noise:ar)
             (decay:ar (* 0.1 (dust:ar [1 1])) 0.3))]
    (buf-comb-c:ar (local-buf (sample-rate) 2)
                   z
                   (x-line:kr 0.0001 0.01 20) 0.2)))


;; 2015.09.09 (like) sad
(defsound test option
  (let [freq (lag  (dt 1 [1000 1800 800 600 2000]) 0.5)]
       (mix (rlpf (switch (lf-pulse 1/3)
                          (pulse [200 500 750])
                          (var-saw [250 650 900])) freq 0.1))))

;; 2015.09.11 (like) broken
(defsound test option
  (let [freq (lin-lin (* (lf-pulse 2.2)
                            (cubed (cubed (lf-tri 0.7))))  -1 1 300 1200)]
       (ringz (mix (var-saw (map #(* freq %) [1
                                              (dt 1   [1.7 1.2 1.25 1.8])
                                              (dt 1/2 [2.1 0.8 1.75 2.5 2.8])])
                            (repeatedly 3 #(rand))
                            (lin-lin (* (sin-osc 2)
                                        (sin-osc 2.5)) -1 1 0.5 1)))
              (dt 1/4 (range 600 1800 100)) 0.3)))

;; 2015.09.12
(defsound test option-d
  (ringz (mix (saw (map #(+ 200
                               (* (dt 4/3 [5 1/4 8 3.8 2.2 1/7]) %))
                           (range 1 20))))
         (* dr (rg-lin (lf-saw 2) 100 1000)) 0.05))

;; 2015.09.12
(defsound test option-d
  (ringz (mix (pulse (map #(+ 300
                                 (* % (dt 2 [30 40 5 70 100])))
                             (range 1 20))))
         (* dr (rg-exp (lf-saw:kr 2 -1) 300 20000)) 0.001))

;; 2015.09.14
(defsound test option
  (let [n 8
        freq 220]
    (splay (map #(* 8 %3
              (if (= (mod %1 2) 0) (/ 1 %1 2) (/ 1 %1))
              (sin-osc %2))
          (iterate inc 1)
          (map #(* freq %) (range 1 n))
          (repeatedly n #(lf-noise0 3))))))

;; 2015.09.14
(defsound test option
  (let [n 8
        freq 220]
    (splay (map #(* 8 %3
                    (/ 1 %1)
                    (sin-osc %2))
                (iterate inc 1)
                (map #(* freq % %2)
                     (range 1 n)
                     (cycle [1
                             (rg-lin (lf-noise0 3) 0.98 1)]))
                (repeatedly n #(lf-noise0 3))))))

;;2015.09.15
(defsound test option
  (let [trig (impulse [1 1.01])
        freq (dq trig [400 800 400 500 700 1200])]
    (+ (* (env-gen (env-perc 0.0 0.008) trig) (white-noise))
       (* (env-gen (env-perc 0.01 0.3) trig) (sin-osc (* freq
                                                         (line 3 1 0.01))))
       (* 1/6 (var-saw (* 4 freq) 0 (rg-lin (sin-osc 1) 0.2 0.7))
          (env-gen (env-perc 0.05 0.5) trig)))))

;; 2015.09.16
(defsound test option
  (let [trig (impulse (dt:kr 1 [1 1 4]))
        freq (dq trig [400 805 578 530 460 250])]
    (splay (+ (map #(* (env-gen (envelope [0 1 1 0]
                                    [0.1 0.1 %2]) trig)
                 (sin-osc (* freq %))) (range 1 8 2)
             [0.4 0.2 0.3 0.5])
        (map #(* (env-gen (envelope [0 1 1 0]
                                    [(* % 1/20) 0 0.4]) trig)
                 (sin-osc (* freq %))) (range 2 9 2))))))

;; 2015.09.16
(defsound test option
  (let [trig (impulse (a2k (dt 1 [1 2 4 8 16])))
        freq (dq trig (range 500 (+ 500 (* 100 31)) 100))
        fenv (env-gen (envelope [0 8 1] [0 0.01]) trig)
        env (env-gen (env-perc 0.05 0.5) trig)
        freq (* fenv freq)]
    (splay (* (+ (* 1/2 (sin-osc (rg-lin (sin-osc 2.4) freq (+ freq 10))))
                 (map #(sin-osc (+ (* 2 freq) % (rand 5)))
                      (range -100 100 10))
                 (map #(sin-osc (+ (* 4 freq) % (rand 10)))
                      (range -100 100 40)))
              env))))

;; 2015.09.16
(defsound test option
  (let [trig (impulse (a2k (dt 2 [4 5 6 8 3 8 4 4 12])))
        freq (dq trig (interleave (range 300 800 100)
                                  (range 200 600 30)))
        fenv (env-gen (envelope [0 8 1] [0 0.01]) trig)
        env (env-gen (env-perc 0.05 0.5) trig)
        freq (* fenv freq)]
    (splay (* (+ (* 1/2 (sin-osc (rg-lin (sin-osc 2.4) freq (+ freq 10))))
                 (map #(sin-osc (+ (* 2 freq) % (rand 5)))
                      (range -100 100 10))
                 (map #(sin-osc (+ (* 4 freq) % (rand 10)))
                      (range -100 100 40)))
              env))))

;; 2015.09.17 (like) reduce
(defsound test option-d
  (let [freq (* dr (dt 1 [6000 5964 440 800 2000 4000 2302 9680]))]
      (sin-osc (reduce (fn [acc _]
                         (* freq (sin-osc acc))) 400 (range 2)))))

(defsound test option-d
  (let [freq (* dr (dt 1 [6000 5964 440 800 2000 4000 2302 9680]))]
      (sin-osc (reduce (fn [acc _]
                         (* freq (sin-osc acc))) 400 (range 3)))))

(defsound test option-d
  (let [freq (* dr (dt 1 [6000 5964 440 800 2000 4000 2302 9680]))]
      (sin-osc (reduce (fn [acc _]
                         (* freq (sin-osc acc))) 400 (range 4)))))

;; 2015.09.18
(defsound test option
  (let [freq [(x-line 1000 6000 10)
              (env-gen (envelope [0 0 8000 0] [1 1 8]))]]
    (* (sin-osc 5.05 [1 0]) (sin-osc 0.58 [0 1])
       (ringz (sin-osc (reduce (fn [acc _]
                                 (* freq (sin-osc acc))) 450 (range 4)))
              freq 0.001))))

;; 2015.09.18
(defsound test option
  (let [trig (impulse [8 8.3] [0 0.3])]
       (bpf (sin-osc (env-gen [(envelope [0 2000 10000] [0 0.05])
                               (envelope [0 500 800] [0 0.08])] trig))
            (rg-lin (sin-osc 8.2) 2000 6000)
            (rg-lin (sin-osc 1/3) 0 1))))

;; 2015.09.19
(defsound test option
  (let [trig (impulse 1)
           freq (dq trig [400 460 686 903 774 939 670])]
       (m-map #(* (sin-osc %)
                  (env-gen (envelope [0 1 1 0] [%2 %3 %4]) trig))
              (map #(* freq %) (iterate inc 1))
              (range 0.01 0.2 0.01)
              (range 0.5 0 -0.05)
              (repeat 0.4 ))))

;;2015.09.20
(defsound test option
  (apply m-map
            #(pulse (lin-lin (clip2 (sin-osc %1 %2) 0.5)  -0.5 0.5 %3 %4))
            (transpose [ [1/4 0 300 500]
                        [1/4 1 500 600]
                        [0.14 1.5 1350 1400]
                        [1/10 1 200 800]])))

;; 2015.09.22
(defsound test option
  (* (pulse (lin-lin (env-gen (envelope [25 100 5] [6 4]))))
        (sin-osc 150)
        (sin-osc (rg-lin (sin-osc 1/4) 10 30))))

;; 2015.09.24
(defsound test option
  (let [freq 500
           trig (impulse 2)]
       (* (+ (sin-osc freq)
             (m-map #(* (/ 1 %) (sin-osc (* freq (+ % %2))))
                    (range 1 5)
                    (map #(+ % (* (lag (latch:ar (line 0 1 10)
                                                 trig) 0.1)
                                  (rand %))) (range 1 5))))
          (env-gen (env-perc 0.05 0.5) trig))))

;; 2015.09.24
(defsound test option
  (let [trig (* (lf-pulse 1) (impulse [12 15]))]
       (* (sin-osc (dq trig (range 300 2200 100)))
          (env-gen (env-perc 0.001 0.5) trig))))

;; 2015.09.25 (broken)
(defsound test option
  (let [freq (rg-lin (lf-noise1 (line 1 10 15)) 100 1200)
           snd (sin-osc freq)]
       (+ snd
          (delay-n snd 3 (line 0.5 3 15))
          (delay-n snd 5 (line 0.5 5 15))
          (delay-n snd 6 (line 0.5 6 15)))))

;; 2015.09.26
(defsound test option
  (let [trig (impulse 2)
           freq (dq trig [500 1000 1000 2000])]
       (* (rlpf (white-noise)
                (env-gen (envelope [0 20000 freq] [1e-05 0.01] EXP) trig)
                (dq trig [0.8 0.1 0.1 0.01]))
          (env-gen (env-perc 0.01 0.1) trig))))

;; 2015.09.27
(defsound test option
  (let [freq (rg-exp (lf-noise1:kr (a2k (x-line 1 100 15))) 100 1500)]
       (+ (sin-osc freq)
          (delay-n (sin-osc (* 2 freq)) 0.5 0.5)
          (delay-n (* (rg-lin (sin-osc 1.4) 0.5 0.8)
                      (pulse (* 4 freq))) 1.5 1.5))))

;; 2015.09.28
(defsound test option
  (let [trig (dust:kr [5 3])
           freq (a2k (rg-lin (lf-noise2 1) 2000 5000))]
       (* 1/6 (ringz (white-noise)
                     (env-gen (envelope [0 500 freq 0 ] [0 0.1 0] EXP) trig)
                     (env-gen (envelope [0 0.1 0.001 0] [0 0.1 0] EXP) trig))
          (env-gen (env-perc 0.01 0.3) trig))))

;; 2015.09.29
(defsound test option
  (let [trig (impulse (x-line:kr 0.5 15 10))
           freq (dq trig [300 373 438 605 638 716 865 966])]
       (+ (* (rlpf (saw freq)
                   (* freq (env-gen (envelope [0 9 0] [0.2 0.4]) trig)))
             (env-gen (env-perc 0.05 0.5) trig))
          (* -1 (sin-osc freq)
             (env-gen (envelope [0 1 0] [0.2 0.3]) trig)
             (env-gen (env-perc 0.05 0.5) trig))
          (* 1/4 (m-map sin-osc (map #(* % freq) [8 11 14 19]))
             (env-gen (env-perc 0.03 0.3) trig)))))

;; 2015.09.30
(defsound test option
  (let [snd (mix (map #(sin-osc %)
                         (map #(dt [1/2 1/2 2/3 1/4 1/4] %)
                              (transpose [[440 503 576]
                                          [487 598 602]
                                          [532 503 560]
                                          [507 626 745]
                                          [532 767 745]]))))
           env (lin-lin (squared (lf-tri [1.2 3.2])) 0 1 0.8 1)]
       (* snd env)))

;; 2015.10.01
(defsound test option
  (let [freq (+ (dt 1 [300 600 400 900])
                   (duty 1/2 0 (drand (range 0 300 100) INF)))]
       (m-map #(sin-osc (* % freq))
              (map #(dt 1/2 %) (transpose [ [1/2 5/3 7/5]
                                           [1/2 4/3 8/5]
                                           [2/3 5/3 7/5]
                                           [1/2 3/3 9/5]])))))

;; 2015.10.02
(defsound test option
  (let [snd (rlpf (white-noise) (rg-lin (sin-osc 1/4) 2000 4000))]
       (* (switch (lf-pulse 1/4 0 0.7) (* 4 (squared snd)) snd)
          (- 1 (* 10000 (lag (dust (rg-lin (lf-tri 0.52 -1) 1 10)) 0.8)))
          (lf-pulse (a2k (dt 4 [1/2 4])) 0 0.8))))

;; 2015.10.02 (like) simple
(defsound test option
  (* (cubed (* 3 (bpf (white-noise) 10000 0.2)))
     (sin-osc (rg-lin (lf-tri 0.1 -1) 40 5120))))

;; 2015.10.02
(defsound test option
  (let [trig (impulse 2)]
       (* (hpf (mix (* 4 (bpf (white-noise)
                              [10000 17510 19868.463]
                              (line 0.9 0.1 10))))
               (* 1000
                  (env-gen (envelope [0 5 20 0]
                                     [0.013 0.05 0.01]) trig)))
          (env-gen (env-perc 0.001 0.2) trig))))

;; 2015.10.03
(defsound test option
  (let [freq (dt 1/4 (shuffle (range 300 800 100)))]
       (mix (map #(* (sin-osc (* % freq))
                     4
                     (/ 1 %)
                     %2)
                 (range 1 14)
                 (map *
                      (repeatedly #(lf-noise2 (ranged-rand 0.1 3)))
                      (cycle [1 0.2]))))))

;; 2015.10.03 (like) sa-
(defsound test option-d
  (let [snd (bpf (white-noise) (* dr [1000 2000 10000]) (rg-exp (lf-saw -1/10 -1) 1 0.01))]
    (-> snd
        (free-verb 1 0.1)
        (comb-l 1/8 1/8 1.2)
        (* 10)
        mix
        tanh)))

;; 2015.10.04
(defsound test option-d
  (let  [snd (mix (* (bpf (white-noise)
                          (map #(rg-lin (lf-noise2 1/4) % (* % 1.8))
                               [200 4000 5000])
                          [0.2 0.1 0.05])
                     [25 2 1]))]
    (* (sin-osc (* dr (lin-exp (latch:ar (lf-tri 1/5 -1) (impulse 2))
                               -1 1 0.01 200)))
       snd)))

;; 2015.10.05 (like) drum roll
(defsound test option-d
  (let [gate (impulse (rg-exp (lf-saw:kr -1/4 1) 1/2 20))]
       (* (+ (* 10 (rlpf (white-noise)
                         (* dr (env-gen (envelope [100 100 750 750] [0.02 0 1])
                                   gate))
                         0.4))
             (* 1/5 (hpf (white-noise)
                         (* dr (env-gen (envelope [12000 0] [0.01])
                                   gate)))))
          (env-gen (env-perc 0.01 0.15) gate))))

;; 2015.10.05 funny
(defsound test option-d
  (let [gate (mk-rythm 0.2 [1/2 1/2 1/4 1/4 1/4 1/4])
        freq (* dr (dq gate (mapcat #(range (* % 100) (* % 500) (* % 100))
                               (shuffle (range 3 8)))))]
       (* (lpf (m-map #(* (lin-exp (lf-noise2:kr 10) 0 1 0.5 1)
                          (sin-osc (* freq (+ % (lin-lin (lf-noise1:kr 10) 0 1 0 0.05)))))
                      (range 1 8 2))
               (* freq (env-gen (envelope [0 5 2] [0 0.05]) gate)))
          (env-gen (adsr 0.1 0 1 0.1) gate))))

;; 2015.10.05
(defsound test option-d
  (let [gate (impulse 2)]
       (* (sin-osc (x-line 700 5000 10))
          (sin-osc (rg-lin (sin-osc 8) 50 250))
          (sin-osc (rg-lin (sin-osc 1/2) 100 1000))
          (env-gen (env-perc 0.05 0.5) gate))))

;; 2015.10.05
(defsound test option-d
  (let [note (->> (map (cycle-fn #(+ % 1)
                                 #(* % 2)) (range 1 15))
                  (map #(+ 200 (* % 50))))
        freq (* dr (dt 1/4 (concat note (reverse note))))]
    (sin-osc freq)) )

;; 2015.10.08
(defsound test option
  (+ (b-low-pass (white-noise) [200 900] 0.2)
     (* (step 3 4)
        (sin-osc (map #(+ % (rg-lin (lf-noise2 10) 0 60))
                      [300 1000]))
        (lf-pulse (a2k (dt 4 [1 1/2]))))))

;; 2015.10.08
(defsound test option
  (splay (let [gate (lf-pulse (a2k (dt [1 1 1/2] [1 2 4])) 0 [0.5 0.4 0.3])
               freq [
                     (dt 1 (take 30 (mapcat (fn [x] [x x]) (iterate #(* 1.4 %) 200))))
                     (dt 1/2 (range 500 600 10))
                     (dt 2 (mapcat (fn [x] [x x]) (range 1600 800 -150)))
                     ]
               fenv (env-gen (envelope [0 8 1 4] [0 0.05 0.2]) gate)]
           (* (rlpf (- (saw freq)
                       (* 0.8 (env-gen (adsr 0.5) gate) (sin-osc (* 2 freq)))
                       (* 0.7 (env-gen (adsr 0.1) gate) (sin-osc (* 3 freq))))
                    (* freq fenv))
              (env-gen (adsr 0.01 0.2 0.5 0.1) gate)))))

;; 2015.10.10
(defsound test option-d
  (let [gate (impulse (a2k (dt 1 [1 2 4])))]
       (lpf (* (pulse 200)
               (sin-osc (* dr (dq gate (concat
                                   (repeat 8 440)
                                   (shuffle (range 400 1200 200))))))
               (env-gen (env-perc 0.05 0.5) gate))
            (+ 10000 (* -1 (env-gen (triangle 0.4 10000) gate))))))

;; 2015.10.12
(defsound test option-d
  (let  [env (lag-ud (lf-pulse 1/3 -2) 0.1 1)
         freq (* dr (dt 8 [440 650]))
         ar-freq (dt 2 [10 300 180 220])]
    (* (+ (sin-osc freq)
          (* 1/2 (sin-osc ar-freq) (sin-osc freq))
          (* 1/4 (lf-tri (* 3 (+ freq ar-freq))))
          (* 1/3 (pulse (* 4 (- freq ar-freq)))))
       env)))

;; 2015.10.12
(defsound test option
  (let [trig (lf-pulse (a2k (dt 6 [1 8])) 0 0.2)
          freq (dq trig [400 501 741 1001 1151 1541])
          env (env-gen (adsr 0.005 0.1 1 (a2k (dt 4 [0.07 0.3]))) trig)
          fenv (lin-exp (- 1 env) 0 1 2 12)]
      (* env (rlpf (saw freq) (* freq fenv) 0.3))))

;; 2015.10.13
(defsound test option
  (let [freq (dt 3 [400 600 1400 3230])]
       (+ (sin-osc (* (sin-osc (rg-lin (sin-osc 1/10) 20 50)) freq))
          (* 1/4 (sin-osc (* 3 (sin-osc (rg-lin (sin-osc 1/9) 600 800)) freq)))
          (* 1/12 (sin-osc (* 5 (sin-osc (rg-lin (sin-osc 1/8) 30 50)) freq))))))

;; 2015.10.13
(defsound test option-d
  (let [trig (impulse 8)
        trig (pattern trig [1 0 1 1 1 0 1 1 1 0 1 0
                            1 0 1 0 1 1 1 1 1 0 1 0
                            1 0 1 0 1 1 1 1 1 0 1 0
                            1 0 1 0 1 0 1 0 1 0 1 0])
        env (env-gen (env-perc 0.0001
                               (v0:kr 0.01 0.04))
                     trig)
        snd (* (mix (sin-osc [5620 7240 9240 11240]))
               (lpf (white-noise) (v0 (line 900 300 12) 1000)))]
    (* env snd 32)))

;; 2015.10.13
(defsound test option-d
  (let [freqs (map #(* % (v0 0.8 1))
                   [1000 6000 14000])
        snd (bpf (white-noise) freqs
                 (x-line:kr 1 0.01 6))]
    (mix (* 10 (comb-l (g-verb snd 1 0.1) 1/8 1/8 1.2)))))

;; 2015.10.14 (some kind of insects)
(defsound test option
  (* (sin-osc (rg-lin (sin-osc 1) 30 64))
     (sin-osc [(dt 3 [1760 1581 1407 1257])
               (dt 2.5 [2640 2333 2126])])
     (sin-osc (rg-lin (sin-osc 1/4) 100 200))
     (env-gen (env-perc 0.05 4) (impulse 1/4 [0 1/2]))))

;; 2015.10.14(star)
(defsound test option
  (let [snd (* (saw (v0 4000 4050))
               (env-gen (env-perc 0.05 0.5)))
        snd2 (comb-c snd 1/5 1/5 2)
        snd3 (comb-c snd2 1/3 1/3 3)]
    (+ snd (* 2/3 snd2) (* 1/3 snd3))))

;; 2015.10.15 (message from space)
(defsound test option
  (let [snd (mix (* (sin-osc [(v0 280 281)
                              (v0 800 810)
                              (v0 1020 1040)
                              (v0 1836 1852)
                              (v0 3096.56 3103.952)
                              (v0 2134.735 2165.257)])
                    (repeatedly 10 #(v0 0.8 1))))]
    (switch (lf-pulse 1/2 0 0.3)
            (* 1/2 (g-verb snd 1 10 0.8))
            (free-verb snd 1 10 0.8))))

;; 2015.10.16
(defsound test option
  (let [trig [(pattern (impulse 8) [1 0 0 0 1 0 0 0 1 1 0 0 1 1 0 0
                                    0 0 0 1 1 1 1 0 0 1 0 0 0 1 0 1])
              (pattern (impulse 8) [0 0 1 0 0 0 1 0 1 1 0 0 1 1 0 0
                                    1 1 0 0 0 1 1 0 0 1 0 0 1 0 1 0])]]
    (* (pulse [(v0 380 400)
               (rg-lin (sin-osc 0.32) 670 800)]
              (rg-lin (sin-osc 10) 0.4 1))
       (env-gen (env-perc 0.03 1/8) trig))))

;; 2015.10.17
(defsound test option
  (let  [freq 400
         ratio (shuffle (reductions * (take 8 (cycle [7/6 4/3]))))
         freq-fn (fn [gate] (* freq (dq gate ratio)))]
    (free-verb (m-map #(sin-osc (freq-fn %))
                      (map #(impulse 1 %) (range 0 1 0.1))))))

;; 2015.10.18(like)
(defsound test option
  (let [noise (white-noise)
        freq (rg-exp (sin-osc:kr [1/3 1/10]) 1000 15000)]
    (switch (lf-tri 1/5)
            (rlpf noise freq 0.1)
            (rhpf noise freq 0.1))))

;; 2015.10.19
(defsound test option
  (m-map #(let [gate (impulse 1/4 %)
                freq (a2k (dt 4 (shuffle (range 400 2000 50))))
                up (a2k (dt 4 (take 10 (cycle [15/14 14/15]))))]
            (* (sin-osc (env-gen (envelope [0
                                            freq
                                            (* freq up)
                                            (* freq 1/2)]
                                           [0 0.5 3]) gate))
               (env-gen (env-perc 0.5 3) gate)))
         (repeatedly 10 #(ranged-rand 0 1))))

;; 2015.10.20
(defsound test option
  (* (pulse (x-line 440 8000 10) 0.5) (lf-pulse (dt:kr 3 [16 4 64]))
     (sin-osc (v0 300 4000))))

;; 2015.10.20 (perc koh)
(defsound test option
  (let [trig (impulse 2)]
    (* 900
       (bpf (saw (dt 1.8 [20 25 30])) 800 (dt 4 [0.1 0.5]))
       (env-gen (env-perc 0 0.03) trig))))

;; 2015.10.21
(defsound test option
  (let [trig (impulse 1 [0 1/2])
        snd (pulse (rg-lin (white-noise) 1500 2550))
        snd2 (lf-tri (rg-lin (white-noise) 1500 2550))]
    (* [snd snd2]
       (env-gen (env-perc 0.04 0.5) trig)
       (lf-pulse 10 0 (a2k (dt 7 [1 0.25]))))))

;; 2015.10.22
(defsound test option-d
  (m-map #(* (sin-osc (rg-lin (lf-noise0 32)
                              %1 (* 1.2 %1)))
             (clip2 (cubed (sin-osc 1/8 %2)) 0.5))
         (range 2000 14000 1000)
         (range 0 6 1/2)))

;; 2015.10.22 (perc: ka)
(defsound test option-d
  (let [gate (impulse 1)
        fenv (env-gen (envelope [0 1800 1400] [0 0.1]) gate)]
    (free-verb (ringz (* (white-noise) gate) fenv 0.04) 0.15 0.3 0.2)))

;; 2015.10.22
(defsound test option-d
  (* 4
     (bpf (saw (rg-lin (lf-saw (rg-lin (lf-saw 100) 100 10000)) 100 10000))
          (line 400 150 0.03) 0.1)
     (env-gen (env-perc 0.01 0.5) :action FREE)))

;; 2015.10.22
(defsound test option-d
  (* 4 (bpf (white-noise) (* dr (line 300 150 0.03)))
     (env-gen (env-perc 0.01 0.1)
              (impulse (dt:kr [3 1/4 3/4] [8 16 0])))))

;; 2015.10.23
(defsound test option
  (let [freq (env-gen (envelope [300 300 400 540 300 530 1500 0]
                                [0.5 1 0.2 0.5 3 0.5 0.1]) :action FREE)]
    (* (+ (* (rg-lin (sin-osc 0.7) 0.8 1) (sin-osc (* 1/2 freq)))
          (* 1/32
             (clip2 (sin-osc freq) (rg-lin (sin-osc 3.2) 0.5 0.7))
             (sin-osc (* 4 freq))))
       (lag-ud (lf-pulse 1 0 (line:kr 0.1 1 8)) 0.03 0.3))))

;; 2015.10.23 (perc po)
(defsound test option
  (* 80
     (env-gen (env-perc 0.0 0.05)
              (impulse (a2k (dt [2 1 1/4] [8 18 0]))
                       [0 1/8]))
     (moog-ff (bpf (white-noise) (rg-lin (sin-osc 1/4) 100 150) 0.01)
              [200 220] 4)))
;; 2015.10.24
(defsound test option-d
  (let [gate (pattern (impulse 8) [1 0 0 0 1 0 0 0 1 1 0 0 1 0 0 0])]
    (* (+ (* 2 (sin-osc (* 100 dr)))
          (* 1/64 (saw (round (rg-lin (lf-noise0 16) 8000 10000)
                              100))))
       (lf-pulse 32)
       (env-gen (env-perc 0.05 0.5) gate))))
;; 2015.10.24
(defsound test option-d
  (* (rlpf (white-noise)
           (* dr (rg-exp (sin-osc:kr 0.353) 500 10000))
           (rg-lin (sin-osc:kr 1/2) 0.1 0.8))
     (lag-ud (lf-pulse 8 0 0.1) 0.05 0.0001)))

;; 2015.10.25
(defsound test option-d
  (m-map (fn [freq phase]
           (let [gate (impulse (a2k (dt 2 [1/4 1 4])) phase)
                 snd (* 1/4 (env-gen (env-perc 0.05 0.5) gate)
                        (+ (rlpf (saw freq)
                                 (* freq (rg-lin (sin-osc 3.2) 2 4)))
                           (lpf (pulse (* freq 4)
                                       (rg-lin (sin-osc 1.3) 0.1 0.5))
                                (* freq 6))))
                 filtered (reduce (fn [acc x]
                                    (allpass-c acc 0.2 (+ 0.01
                                                          (rand 0.04))
                                               1))
                                  snd
                                  (repeatedly 5 #(rand 0.04)))]
             (+ snd filtered)))
         (reverse (range 300 2800 200))
         (range 0 1 0.1)))


;; 2015.10.25
(defsound test option-d
  (m-map (fn [phase]
           (apply m-map #(* %4
                            (sin-osc (+ % (* 1000 phase)))
                            (env-gen (env-perc %2 %3) (impulse 1 phase)))
                  (transpose [[400 0.2 0.3 1]
                              [473.368 0.1 0.05 1]
                              [600.691 0.05 0.3 1]
                              [1200 0.01 0.4 0.5]
                              [4420 0.01 0.7 0.01]])))
         (range 0 1 1/6)))

;; 2015.10.26 (difficult to use)
(defsound test option-d
  (let [seed-freq (rg-lin (sin-osc:kr (rg-exp (lf-saw:kr 1/2) 0.1 10)) 300 600)
        freq (lin-lin (latch:ar (sin-osc 1/10) (impulse 8))
                      -1 1
                      (* 8 seed-freq) (* 8.5 seed-freq))]
    (* 1/4 (sin-osc (reduce (fn [acc _]
                              (* dr freq (sin-osc acc))) seed-freq (range 3))))))

;; 2015.10.27 (reduce multiple)
(defsound test option-d
  (let [seed-freq-freq (lin-exp (m-map lf-pulse [4 2.3 9.1]) 0 1 1 32)
        seed-freq (rg-lin (lf-pulse seed-freq-freq) 800 900)
        freq (lin-lin (m-map lf-pulse [1/10 0.34 0.25 2.5])
                      0 1
                      (* 4 seed-freq) (* 10 seed-freq))]
    (* 1/4 (sin-osc (reduce (fn [acc _] (* dr freq (sin-osc acc)))
                            seed-freq (range 3))))))

;; 2015.10.27 (like?)
(defsound test option-d
  (let [seed-freq-freq (lin-exp (m-map lf-pulse [4 2.3 9.1]) 0 1
                                (dt:kr 4 [4 8])
                                (dt:kr 4 [6 12]))
        seed-freq (rg-lin (lf-pulse seed-freq-freq) 200 1200)
        freq (lin-lin (m-map lf-saw [1/10 0.34 0.25 2.5])
                      0 1
                      (* 4 seed-freq) (* 8 seed-freq))]
    (leak-dc (* 1/4 (lf-tri (reduce (fn [acc _] (* freq dr (sin-osc acc)))
                                    seed-freq (range 4)))))))

;; 2015.10.27
(defsound test option-d
  (m-map
   #(let [gate (impulse 1/2)
          freq (dt:kr [2 2 1 1 1/4 1/4 1/4] [400 480 780 200])
          freq [(* 1.01 freq) (* 0.99 freq)]
          fenv (env-gen (envelope [0 4 1 2] [0 % 0.4]) gate)
          env (env-gen (envelope [0 1 1 0] [0 0.1 0.7]) gate)
          width (rg-lin (sin-osc [2 2.1] [ (rand 1) (rand 1)]) 0.1 0.5)
          snd (* (rlpf (pulse freq width) (* freq fenv)) env)
          gate2 (impulse 1 1/8)
          freq2 (env-gen (envelope [0 4000 freq 10000]
                                   [0 0.05 0.8] EXP) gate2)
          env2 (env-gen (env-perc 0.5 0.1) gate2)
          snd2 (* 1/4 env2 (pulse (* (sin-osc 17) freq2)))]
      (free-verb (+ snd snd2) 0.33 0.4 0.5))
   [0.2 0.3]))

;; 2015.10.28 (like rythm)
(defsound test option
  (let [freq (rg-lin (sin-osc 1/4) 78 80)
        freq [(* 1.1 freq) (* 0.9 freq)]
        freq2 [160 160.5]
        gate (impulse 2)
        fenv (env-gen (envelope [0 1.5 0.4] [0 0.005]) gate)
        gate2 (impulse 8) ]
    (+ (* 2 (sin-osc (* fenv freq))
          (env-gen (envelope [0 1 1 0] [0.00 0.1 0.000]) gate))
       (* (lf-pulse 1 0 1/6)
          (lf-pulse 16 0 0.1)
          (rlpf (lf-tri (rg-lin (sin-osc 3) freq2 (* 1.5 freq2)))
                (rg-lin (sin-osc 1/4) 300 3000) 0.8))
       (* 1/128 (rhpf (* (white-noise)
                         (lf-saw [2000 500]))
                      (env-gen (envelope [0 10000 100] [0 0.001] EXP) gate2)
                      0.1)
          (env-gen (env-perc 0.00001 0.005) gate2)))))

;; 2015.10.29
(defsound test option
  (let [gate (lf-pulse 1 0 (a2k (dt [8 2] [1/4 1])))
        freq (dq gate [240 2000 2200])
        fenv (env-gen (envelope [0 10 0.5 1] [0 0.01 0.05]) gate)
        fenv2 (env-gen (envelope [0 0 900 300 320 15000] [10.5 0 0.1 0.2 0.6]))]
    (+ (* 1/2 (saw (* fenv freq (rg-lin (sin-osc 100) 0.5 1)))
          (env-gen (adsr :release 0.2) gate)
          (- 1 (* (lf-pulse 8) (step 8))))
       (* (lf-tri [fenv2 (* 1.1 fenv2)])
          (- 1 (* (lf-pulse 11) (step 11)))))))

;; 2015.10.29 (perc dash)
(defsound test option
  (let [gate (impulse (a2k (rg-lin (sin-osc 1.5) 3.4 4.2)))]
    (map (fn [freq] (* (bpf (switch (lf-pulse 1/4)
                                    (pink-noise)
                                    (brown-noise))
                            (env-gen (envelope [0 freq (/ freq 3)] [0 0.02]) gate)
                            0.8)
                       4
                       (env-gen (env-perc 0.01 0.1) gate)))
         [(dq gate [600 300 280 200])
          (dq gate [200 300 880 240])])) )

;; 2015.10.29
(defsound test option-d
  (* (switch (lf-pulse 2) (white-noise) (pink-noise))
     (lf-saw:ar (rg-exp (sin-osc:kr 1/4 1) 10 10000))))

;; 2015.10.30 (music)
(defsound test option
  (let [gate (impulse 10)
        gate4 (impulse 5)
        freq (dq gate4 [120 180])
        fenv (env-gen (envelope [0 8 1 2] [0 0.1 0.01]) gate4)]
    (+ (* (rlpf (saw freq) (* freq fenv)))
       (* 3 (sin-osc 80) (env-gen (env-perc 0.01 0.3) (stepper gate 3)))
       (* (rhpf (saw (* 3000 (sin-osc 220)))
                (+ (rg-exp (sin-osc:kr 1/5) 100 4000)
                   (env-gen (envelope [0 8000 1000] [0 0.01]) gate)) 0.1)
          (env-gen (env-perc 0.01 0.1) gate)))))

;; 2015.10.31 (music)
(defsound test option
  (let [tempo 0.7
        gate (impulse tempo 1/2)
        gate2 (impulse (* 4 tempo) )
        base-freq (dt 4 [50 70 40])
        fenv (env-gen (envelope [0 5 0.8 1] [0 0.03 0.2]) gate)]
    (+ (* (sin-osc (* 110 fenv)) (env-gen (env-perc 0.05 0.5) gate))
       (* (bpf (white-noise) (+ (* 50 base-freq) (dq gate2 (shuffle (range 6000 7000 100)))) 0.1)
          (env-gen (envelope [0 1 1 0] [0.00001 0.05 0.001]) gate2))
       (let [gate (impulse (* 12 tempo))]
         (* 1/2 (+ (pulse base-freq)
                   (* 1/4 (saw (* base-freq (dq gate (range 20 60 10))))))
            (env-gen (envelope [0 1 1 0] [0.0001 0.05 0.1]) gate))))))



;; 2015.11.01 (rythm)
(defsound test option
  (let [gate (impulse (dt:kr 1 [2 2 8 2 2 8 2 2 16]) [0 1/16])
        release (dt:kr 1 [0.03 0.03 0.01])
        freq 330
        fenv (env-gen (envelope [0 (* 1.7 freq) freq] [0 release]) gate)]
    (* (sin-osc fenv) (env-gen (env-perc 0.0005 release)  gate))))

;; 2015.11.01 (rythm)
(defsound test option
  (+  (let [gate (impulse 1 [0 2/3])
            m-env (map #(env-gen %1 %2)
                       [(envelope [0 0 1] [0 0.1]) (envelope [0 1 0] [0 0.2])]
                       gate)]
        (* (sin-osc 100)
           (- 1 (* m-env (lf-pulse 32 0 0.5)))
           (env-gen (env-perc 0.05 0.5) gate)))
      (let [gate (* (lf-pulse 1/2 0.5) (impulse 8 1/3))]
        (* (bpf (white-noise) [(dq gate (range 4000 900 -1000))
                               (dq gate [300 8000])] [0.64 0.1]) gate))))


;; 2015.11.01 (door and window)
(defsound test option
  (let [gate (* (lf-pulse 1/2 0 7/8) (impulse 8))
        snd (* (klank (transpose [[111 1 0.8]
                                  [528 0.4 0.2]
                                  [588 0.4 0.2]
                                  [2552 0.05 0.2]
                                  [392 0.4 0.1]]) (white-noise))
               (env-gen (env-perc 0.001 0.17) gate))]
    snd))

;; 2015.11.02
(defsound test option
  (let [gate (impulse 24)
        gate1 (stepper gate 0 0 3)
        gate2 (delay-n:kr (stepper gate 0 0 (* 3 24) 1) 2 2)
        gate3 (stepper gate 0 0 2)]
    (+ (* 6 (bpf (brown-noise) (env-gen (envelope [0 2400 600] [0 0.005]) gate1) 0.5)
          (env-gen (env-perc 0.0001 0.05) gate1))
       (* 4 (lf-pulse 1/2 1/2) (sin-osc 1664) (saw 4000)
          (env-gen (env-perc 0.01 0.001) gate3))
       (free-verb (* 1/10 (saw 0) (brown-noise) (env-gen (env-perc 0.02 0.1) gate2))
                  (env-gen (envelope [0 1 0.7] [0 0.2]) gate2)
                  0.05))))

;; 2015.11.03 (quite water)
(defsound test option
  (let [gate (impulse 1)
        freq (dt 1 [800 1200 3400 5000])]
    (apply m-map #(* %2 (sin-osc (* % freq))
                     (env-gen (env-perc %3 %4) gate))
           (transpose [[1.8  1   0.02 0.001]
                       [1.3  1/4 0.01 0.03]
                       [1.1  1/2 0.03 0.03]
                       [1    1/4 0.055 0.15]]))))
;; 2015.11.03 (quite water)
(defsound test option
  (let [gate (impulse 1)
        freq (dt 1 [800 1200 3400 5000])]
    (apply m-map #(* %2 (sin-osc (* % freq))
                     (env-gen (env-perc %3 %4) gate))
           (transpose [[3.3  1   0.001 0.2]
                       [1.3  1/4 0.05 0.3]
                       [1.1  1/2 0.01 0.1]
                       [1    1/2 0.1 0.5]]))))
;; 2015.11.03  (hard)
(defsound test option
  (let [gate (impulse 1)
        freq (dt 1 [800 1200 3400 5000])]
    (apply m-map #(* %2 (ringz (brown-noise) (* % freq) 0.2)
                     (env-gen (env-perc %3 %4) gate))
           (transpose [[10   1   0.0001 0.001]
                       [4.5  1/2 0.01 0.1]
                       [2.5  1/2 0.02 0.05]
                       [1    2/3   0.01 0.04]]))))

;; 2015.11.03
(defsound test option
  (let [gate (impulse 1)
        freq (dt 1 [800 1200 3400 5000])]
    (apply m-map #(* %2 (ringz (brown-noise) (* % freq) 0.2)
                     (env-gen (env-perc %3 %4) gate))
           (transpose [[10.3  1   0.001 0.009]
                       [1    2/3   0.01 0.04]]))))

;; 2015.11.04
(defsound test option
  (apply m-map #(* (sin-osc %)
                   (env-gen (env-perc %2 %3) (impulse %4 %5)))
         (transpose [[400  3   8 1/10 0]
                     [700  2   8 1/10 0]
                     [800  1   4 1/10 8/10]
                     [1500 1   10 1/10 6/10]
                     [600  8   10 1/10 5/10]
                     [440  3   1  1/10 4/10]
                     [2300 0.5 1  1/10 3/10]
                     [3300 0.5 4  1/10 3/10]])))

;; 2015.11.04
(defsound test option
  (let [rate (dt:kr 1 [1/3 1/5 1/5 1/10 1/50 1/100])]
    (apply m-map #(* (sin-osc %)
                     (env-gen (env-perc (* rate %2) (* rate %3)) (impulse (/ %4 rate) %5)))
           (transpose [[400  3   8 1/10 0]
                       [700  2   8 1/10 0]
                       [800  1   4 1/10 8/10]
                       [1500 1   10 1/10 6/10]
                       [600  8   10 1/10 5/10]
                       [440  3   1  1/10 4/10]
                       [2300 0.5 1  1/10 3/10]
                       [3300 0.5 4  1/10 3/10]]))))

;; 2015.11.04 (thai)
(defsound test option
  (let [seed-freq-freq 2
        gate (lf-pulse seed-freq-freq)
        seed-freq (lin-lin gate 0 1 800 900)
        freq (lin-lin (m-map lf-pulse [1/10 0.34 0.25 2.5])
                      0 1
                      (* 4 seed-freq) (* 10 seed-freq))]
    (+ (* (line 0 1 10) 1/8 (sin-osc (reduce (fn [acc _] (* freq (sin-osc acc)))
                                             seed-freq (range 3))))
       (let [base-freq (map #(* % (dq gate [100 80])) [1 1.01])
             fenv (env-gen (envelope [0 8 1] [0.0001 0.01]) gate)]
         (* (sin-osc (* base-freq fenv) ) (env-gen (env-perc 0.001 0.1) gate)))
       (let [gate (impulse 8)]
         (* (lf-pulse 1) 100 (bpf (brown-noise) [2500 2510] 0.01) gate)))))

;; 2015.11.05 (base line)
(defsound test option
  (let [gate (pattern (impulse 8) [1 1 0 0 1 0 1 0 1 0 0 0 ])
        freq (lag-ud (dq gate (flatten [(repeat 3 [100 80 130 60 80])
                                        [180 120 120 90 60]])) 0.1 0.2)]
    (+ (* (sin-osc [ (* 1.01 freq)
                    (* 0.99 freq)])
          (env-gen (envelope [0 1 1 0] [0.001 0.2 0.5]) gate))
       (let [gate (impulse 4)
             vol (dq gate [1 1/16 1/12 1/8])
             release (dq gate [0.03 0.01 0.015 0.02])]
         (* vol (white-noise) (env-gen (env-perc 0.001 release) gate)))
       (let [gate (impulse 1/6)
             freq-max (dq gate [8000 7000 7200 9000])
             freq (env-gen (envelope [0 400 freq-max] [0 0.4]) gate)
             amp (env-gen (envelope [1 1 1/8] [0.2 0.1]) gate)]
         (* (m-map #(saw (* % freq)) [1 1.01 1.1]) amp)))))

;; 2015.11.06
(defsound test option
  (let [gate1 (lf-pulse 1 3/4)
        gate (* gate1 (lf-pulse 16))
        freq (latch:ar (lin-lin (m-map lf-pulse [1 0.1 1/3]) 0 1 200 2000) gate)]
    (+ (* 1/2 (pulse [(* 1.01 freq) (* 0.99 freq)]) gate)
       (* 1/4 (m-map #(sin-osc (* % freq)) [1 3/2 7/4 1/4])
          (env-gen (env-perc 0.2 1) gate1)))))

;; 2015.11.06
(defsound test option
  (let [gate (impulse 1)
        freq (dq gate (range 300 1800 200))
        fenv (env-gen (envelope [0 3000 600] [0 0.5]) gate)
        fenv2 (env-gen (envelope [0 50 10] [0 0.5] EXP) gate)]
    (* (rlpf (saw freq) (rg-lin (sin-osc fenv2) 600 fenv))
       (env-gen (env-perc 0.01 1) gate))))

;; 2015.11.07
(defsound test option
  (let [gate (impulse 2)]
    (apply m-map #(* %5 (rlpf (white-noise) %1 %2) (env-gen (env-perc %3 %4) gate))
           (transpose [[1000 0.1 0.0001 0.1 1]
                       [1800 0.1 0.01 0.05 1/2]
                       [200 0.01 0.001 0.02 4]]))))

;; 2015.11.07
(defsound test option
  (map (fn [detune] (let [gate (impulse 2)]
                      (* (apply m-map #(* %4
                                          (saw (* (rg-lin (sin-osc 10 detune) 0.99 1.01) %1))
                                          (env-gen (env-perc %2 %3) gate))
                                (transpose [[2200 0.001 0.05 1]
                                            [1600 0.01  0.5 1/2]
                                            [ 950 0.1   0.2 1/4]
                                            [ 450 0.18  0.0 1/4]]))
                         (lf-pulse 12 0 (dt:kr 15/5 [1 1 0.75 0.25 0.18])))))
       [0 1.5]))

;; 2015.11.07
(defsound test option
  (let [gate (impulse 1)]
    (apply #(* (sin-osc %) (lf-pulse %2) (rg-lin (lf-saw %3) 0 1))
           (map #(dq gate (vector %)) (transpose [[400 100 180]
                                                  [480 100 100]
                                                  [480 60 100]
                                                  [880 60 100]
                                                  [880 60 300]
                                                  [880 100 150]
                                                  [880 300 150]
                                                  [200 300 150]
                                                  [200 100 180]])))))

;; 2015.11.07
(defsound test option
  (+ (let [gate (impulse (dt:kr 1/8 [8 2 1 12 12 2 2 1]))
           fenv (env-gen (envelope [0 12000 300] [0 0.002]) gate)]
       (* (bpf (white-noise) fenv 0.3)
          8 (env-gen (env-perc 0.0005 0.1) gate)))
     (let [gate (pattern (impulse 8) [0 0 0 0 0 0 0 0
                                      0 0 0 1 0 0 0 0
                                      0 0 0 0 0 0 0 0
                                      0 0 0 0 0 1 1 0])]
       (* (mix (bpf (white-noise) [4000 9200 10000 12000] 0.05))
          8 (env-gen (env-perc 0.0005 0.3) gate)))))

;; 2015.11.09
(defsound test option
  (let [gate (impulse 1)
        freq (dq gate [400 100 350 120])
        fenv (lin-lin (env-gen (env-perc 0.0001 0.025) gate) 0 1 freq (* 3 freq))
        gate2 (impulse 1 (dt:kr 3 [1/2 3/4 1/4]))
        fenv2 (lin-exp (env-gen (env-perc 0.01 0.1) gate) 0 1 (* 3 freq) (* 1/2 freq))]
    (+ (* (sin-osc fenv) (env-gen (env-perc 0.05 0.5) gate))
       (* 2 (bpf (white-noise) fenv2 0.3)
          (env-gen (envelope [0 1 1 0] [0.0001 0.05 0.01]) gate2)))))

;; 2015.11.09
(defsound test option
  (let [gate (impulse (dt:kr [4 3] [4 6]))
        freq (dq gate (flatten [(take 12 (iterate (cycle-fn #(* % 3/2)
                                                            #(* % 5/7)) 400))
                                (take 6 (iterate #(* % 5/8) 1500))]))
        fenv [(env-gen (envelope [0 1.1 1] [0 0.01]) gate)
              (env-gen (envelope [0 0.9 1] [0 0.01]) gate)]]
    (* (sin-osc (* fenv freq))
       (env-gen (env-perc 0.05 0.5) gate))))


;; 2015.11.10
(defsound test option
  (let [gate (impulse 1/2)]
    (apply map #(* (lf-pulse % %2 %3) (saw %4) (saw %5) (sin-osc %6))
           (transpose [[1/2 0 1/4 1800 500 80]
                       [1/2 (dt:kr 1 [0 1/4 2/3]) 1/4 1600 660 120]]))))

;; 2015.11.10 (like denki)
(defsound test option
  (let [gate (impulse 1/2)]
    (apply map #(* (lf-pulse % %2 %3) (saw %4) (saw %5) (sin-osc %6))
           (transpose [(map #(dq gate %) [[1/2 1/2 1] [0 1/2]   [1/8 1/4 1/4 1/16]
                                          [1800 1900 2700] [500 700] [80 120]])
                       (map #(dq gate %) [[1/2 1/2 1] [1/4 3/4]  [1/4 1/8]
                                          [1800 1900 2700] [500 600] [100]])]))))

;; 2015.11.10 (love)
(defsound test option
  (let [gate (lf-pulse 4 3/4)
        big-freq (switch (lf-pulse 1/4 0 3/4) (dt gate (shuffle (range 300 400 20)))
                         (rg-exp (sin-osc 1 [0 1]) 250 1200))
        small-freq (rg-lin (lf-pulse 8) 500 650)]
    (* (sin-osc (+ big-freq small-freq [-10 10]))
       (env-gen (envelope [0 1 1 0] [0.01 0.05 0.08]) gate))))

;; 2015.11.10
(defsound test option
  (let [gate (pattern (impulse 16 [0 1/2]) [1 0 0 1 1 0 1 0 0 1 1 1])
        freq (switch (lf-pulse 1/8)
                     (+ (dq gate [600 500 530 800 600 480]) (rg-lin (lf-noise0 1) 100 300))
                     (dq gate (range 300 (* 8 300) 300)))]
    (* 1/4 (+ (* (pulse freq (v0 0.5 0.9))
                 (env-gen (envelope [0 1 1 0] [0.001 0.1 0.1]) gate))
              (let [gate2 (stepper gate 0 0 4 1)
                    freq (dq gate2 [350 250])
                    fenv (env-gen (envelope [0 1 1.3] [0 0.5] EXP) gate2)]
                (* 1/40 (ringz (saw (* fenv [(* 1.01 freq)
                                             (* 0.99 freq)])) (* freq fenv fenv) 0.03)))))))
;; 2015.11.11
(defsound test option
  (m-map #(let [gate (impulse 1 %)]
            (* (saw 400)
               (sin-osc (lin-lin (env-gen (envelope [0 0 1] [0 0.8] EXP) gate)
                                 0 1 %2 %3))
               (env-gen (envelope [0 1 1 0] [0.1 0.4 0.3]) gate)))
         (range 0 1 0.2)
         [100 5000 3000 1000 500]
         [10000 100 100 2000 20000]))

;; 2015.11.12
(defsound test option
  (let [gate (impulse 3)]
    (* (m-map #(rlpf (white-noise) (* %
                                      (dq gate (range 7 0 -1))
                                      (env-gen (envelope [0 2 1] [0 0.1]) gate))
                     (env-gen (envelope [0 0.01 0.3] [0 0.2] EXP) gate)) [300 420 480])
       (env-gen (env-perc 0.05 0.15) gate))))

;; 2015.11.12
(defsound test option
  (m-map #(let [gate (lf-pulse 1 (+ % (rand 0.2)) 1/8)]
            (* 32 (saw %3) (saw %2) (var-saw (env-gen (envelope [0 10 %4] [0 1/8]) gate))
               (lag-ud gate 0.02 0.1) ))
         (range 0 1 2/8)
         (shuffle (range 100 800 100))
         (shuffle (range 10 80 10))
         (conj (repeat 6 20) 1000 )))

;; 2015.11.12
(defsound test option
  (let [gate (impulse 1)
        gate2 (* (lf-pulse 1/4 1/2 1/4) (impulse 4))
        fenv (env-gen (envelope [0 8 1] [0 0.01]) gate)
        freq (demand gate2 0 (drand [800 1200 1000 800] INF))
        freq2 (env-gen (envelope [0 100 8000] [0 3]) gate)]
    (+ (* (sin-osc (* fenv 180)) (env-gen (env-perc 0.001 0.08) gate))
       (* (dq gate [1/4 1/16])
          (env-gen (env-perc 0.9 0.1) gate) (m-map #(lf-tri (* % freq2)) [1 3/2 5/4 6/5]))
       (* (saw freq) (env-gen (env-perc 0.01 0.03) gate2 ))
       (* 1/4 (hpf (white-noise) 2000) (env-gen (env-perc 0.01 0.04) gate2 (lf-pulse 1/8 0.48))))))


;; 2015.11.14 (rythm)
(defsound test option-d
  (let [freq 8
        delay (duty:kr (/ 1 freq) 0
                       (dseq (repeatedly 4 (fn [] (choose (map #(/ 1 %) (range 4 6))))) INF))
        gate (impulse freq delay)
        amp (dq gate (flatten (flatten (repeat 2 [(repeat 3 [1 1/4 1/4 1/4])
                                                  [1.3 0 1/2 0]]))))
        bpf-freq (* dr (dq gate (range 1000 1100 25)))]
    (* amp 20 (bpf (white-noise) bpf-freq 0.3) (env-gen (env-perc 0.005 0.05) gate))))

;; 2015.11.15
(defsound test option
  (map (fn [xs] (apply (fn [freq seed-freq gate]
                         (* 1/8 (sin-osc (reduce (fn [acc _] (* freq (sin-osc acc)))
                                                 seed-freq (range 4)))
                            (env-gen (envelope [0 1 1 0] [0 1/8 0.001]) gate)))
                       xs))
       [[5840 2652 (pattern (impulse 4) [ (repeat 3 [1 0 0 0])
                                         [1 1 1 0]])]
        [5840 3978 (pattern (impulse 4) [[0 0 0 1]
                                         [0 0 0 0]
                                         [0 1 0 0]])]]))

;; 2015.11.15
(defsound test option
  (let [freq 3172
        gate (impulse 1)
        t 1/2
        seed-freq (env-gen (envelope [0 1000 3500] [0 t]) gate)]
    (* 1/8 (sin-osc (reduce (fn [acc _] (* freq (sin-osc acc)))
                            seed-freq (range 3)))
       (lf-pulse 1 0 t))))

;; 2015.11.15
(defsound test option
  (let [gate (impulse 1)
        t 1/2
        freq (latch:ar (env-gen (envelope [0 4000 7000] [0 t] EXP) gate) (impulse 20))
        seed-freq 1660]
    (* 1/8 (sin-osc (reduce (fn [acc _] (* freq (sin-osc acc)))
                            seed-freq (range 3)))
       (lf-pulse 1 0 t))))

;; 2015.11.15 (like)
(defsound test option
  (splay (map (fn [freq phase] (let [gate (impulse 1/6 phase)]
                                 (* (let [vib (rg-lin (sin-osc 3.3) 0.999 1)]
                                      (m-map #(* %2 (rg-lin (lf-tri 0.8 %3) 0.8 1)
                                                 (sin-osc (* % freq vib)))
                                             [1 3/2 7/4 2 37/16]
                                             [1 1 2/3 1/4 1/3]
                                             (shuffle (n-range 0 2 5))))
                                    (env-gen (envelope [0 1 1 0] [0.6 5 1]) gate))))
              (shuffle (n-range 300 1800 6))
              (n-range 0 1 5))))

;; 2015.11.15 (rythm chu-pi)
(defsound test option
  (let [gate (pattern (impulse 12) [1 1 0 0 1 0 0 1 1 1 0 0])
        t 0.0086
        freq (env-gen (envelope [0 1.5 10000] [0 t]) gate)]
    (* [(bpf (gray-noise) freq 0.1)
        (sin-osc freq)]
       (env-gen (env-perc t 0.001) gate))))

;; 2015.11.16 (like)
(defsound test option
  (let [freq (+ 500 (* (env-gen (envelope [0.01 10000] [7] EXP) (step 13))))
        mod-freq (lin-exp (m-map lf-pulse [1/4 1/3 1/10]) 0 1 33 300)
        index (rg-lin (lf-tri 1/2) 3 15)]
    (* (lf-tri 1/8) (pm-osc freq mod-freq index))))

;; 2015.11.19
(defsound test option
  (* (sin-osc [70 182])
     (saw (* [30 32.5] (env-gen (envelope [0 1 2] [0 4]) (impulse 1/4))))
     (lf-tri 1/8)))

;; 2015.11.23
(defsound test option-d
  (let [gate  (pattern (impulse 8) [1 0 0 0 1 0 0 0 1 0 0 0 0 1 1 1 0 1 0 1])
        gate2 (pattern (impulse 8) [0 0 1 0 0 0 1 0 0 0 1 0 1 0 1 1 0 0 1 0])]
    (+ (* (sin-osc 440) (env-gen (env-perc 0.02 0.15) gate))
       (* (sin-osc 660) (env-gen (env-perc 0.01 0.25) gate2)))))

;; 2015.11.28
(defsound test option-d
  (let [gate (pattern (impulse 8) (flatten [ (repeat 3 [1 0 0 0])
                                            [1 1 1 0]
                                            (repeat 3 [1 0 0 0])
                                            (repeat 3 [1 1 1 0])
                                            [0 0 1 0]]))
        fenv (* dr (env-gen (envelope [0 1000 1000 5000 4000]
                                 [0 0.08 0 0.05]) gate))
        rq-env (env-gen (envelope [0 0.08 0.2] [0 0.1] :exp) gate)]
    (* 4 (bpf (white-noise) fenv rq-env) (env-gen (env-perc 0.01 0.2) gate))))

;; 2015.11.28
(defsound test option
  (m-map #(let [freq %1
                gate (step %2)]
            (* gate (sin-osc (* freq (lag (latch (env-gen (envelope [1 1.2] [10]) gate)
                                                 (impulse 4))
                                          0.5)))))
         [4000 4500 6000 8200 8800]
         [1 4 6 8 8]))
;; 2015.11.29
(defsound test option
  (let [freq (env-gen (envelope [500 500 600 2400 2400 2000]
                                [2 1 0 2 5]) :action FREE)]
    (* 100 (m-map #(* (/ 1 %) (bpf (white-noise) (* % freq) 0.004))
                  (range 1 20)))) )

;; 2015.11.30
(defsound test option
  (let [freq (dt:kr 3 [2 4 8])
        gate (impulse freq)
        arr [400 530 1220 4020]]
    (m-map #(* (sin-osc %)
               (env-gen:ar (env-perc %2 %3) gate)
               %4)
           arr
           (repeatedly (count arr) #(rg-lin (lf-noise0 freq) 0.01 0.1))
           (repeatedly (count arr) #(rg-lin (lf-noise0 freq) 0.01 0.1))
           (repeatedly (count arr) #(rg-lin (lf-noise0 freq) 0.5 1)))))

;; 2015.12.07
(defsound test option-d
  (let [gate (impulse:kr (dt:kr 1 [2 1 2 2 4 1]))
        n 10
        freq (dq gate (shuffle [400 580 328 1050 520]))
        freq2 1]
    (m-map #(* 5
               (sin-osc (* dr freq %))
               (env-gen:ar (env-perc %2 %3) gate)
               %4)
           (repeatedly n #(rg-lin (lf-noise0 freq2) 0.98 1.02))
           (repeatedly n #(rg-lin (lf-noise0 freq2) 0.01 0.2))
           (repeatedly n #(rg-lin (lf-noise0 freq2) 0.01 0.2))
           (repeatedly n #(rg-lin (lf-noise0 freq2) 0.5 1)))))

;; 2015.12.08(サーバーが落ちる)
(defsound test option-d
  (let [trigger (impulse 4)
        freq (* dr 150)
        gate (switch (pulse 1/4)
                     (pattern trigger [1 0 1 0 1 1 0 0 0 0 1 0])
                     (pattern trigger [0 0 0 1 0 0 0 1 0 1 0 0]))]
    (* (+ (sin-osc freq) (* 1/4 (saw (* 8 freq))))
       (env-gen:ar (env-perc 0.05 0.5) gate))))

;; 2015.12.08
(defsound test option-d
  (let [gate (impulse [2 1.5] [0 1/4])
        env (env-gen (env-perc 0.25 0.05) gate)
        fenv (env-gen (envelope [100 20000 1000] [0.01 0.1]) gate)]
    (* (bpf (white-noise) (* dr (+ fenv (rg-exp (lf-noise1 16) 100 8000))) 0.4)
       env)))

;; 2015.12.11
(defsound test option
  (let [gate (impulse 1)
        [note rythm] (transpose [[0.1 0.1]
                                 [0.3 0.05]
                                 [0.1 0.2]
                                 [0.5 0.1]
                                 [1 0.1]
                                 [0.1 0.02]
                                 [0.8 0.1]
                                 [0.7 0.1]
                                 [1 0.1]
                                 [0 0.5]])
        min-freq (+ 100 (* (env-gen (envelope [0 4000] [5]) (step 4))))
        freq (lin-exp (env-gen (envelope note (butlast rythm) ) gate) 0 1 min-freq 10000)]
    (sin-osc freq)))

;; 2015.12.12
(defsound test option-d
  (let [freq (* dr (lin-lin (m-map lf-pulse (range 1 7)) 0 1 100 1000))]
    (switch (lf-pulse (dt:kr 2 [1/2 1/2 1/2 8 1]))
            (sin-osc freq)
            (saw freq))))

;; 2015.12.12 (rythm)
(defsound test option-d
  (let [gate (impulse (map #(dt:kr 1/2 %) (transpose [[1 4]
                                                      [1 4]
                                                      [1 4]
                                                      [4 2]
                                                      [2 4]
                                                      [1 12]
                                                      [1 4]
                                                      [8 8]
                                                      [8 1]
                                                      [1 8]]))
                      [0 1/2])]
    (* [1 1/2]
       (m-map #(rlpf (white-noise) (* dr %) 0.2)
              [320 411 500 1050 1250 1530 9000])
       (env-gen (env-perc 0.01 0.1) gate))))

;; 2015.12.14 (like string and echo)
(defsound test option-d
  (let [freq (* dr (dt 1 [100 80 120 60 90]))
        gate (* (lf-pulse 1) (impulse (dt:kr 1 [8 8 8 12])))]
    (free-verb (comb-n (* (white-noise)
                          (env-gen (env-perc 0.0001 0.05) gate))
                       (/ 1 freq)
                       (/ 1 freq)
                       0.2)
               0.1
               0.8)))

(defsound test option
  (let [gate (impulse 1)
        freq (line 200 40 10)]
    (m-map #(comb-n (* (white-noise)
                       (env-gen (env-perc 0.0001 0.02) gate))
                    (/ 1 (* freq 0.3))
                    (/ 1 (* freq % ))
                    %2)
           (range 0.1 1 0.2)
           (range 0.01 0.2 0.06))))

(defsound test option
  (* 12 (m-map #(let [gate (impulse 1 %)]
                  (m-map (fn [detune]
                           (let [freq (+ 440 detune)]
                             (comb-l (* (lf-noise2 3000)
                                        (decay2 gate 0.008 0.04))
                                     (/ 1 freq)
                                     (/ 1 freq)
                                     1)))
                         [0 -0.5 0.4]))
               (range 0 0.01 0.001))))

;; 2015.12.23
(defsound test option
  (let [gate (impulse 1 [0 1/2])
        fenv (env-gen (envelope [0 1 0.25] [0 0.2]) gate)
        freq (* (dt 1 [400 50 200 800 1500]) fenv)
        delay-time (/ 1 (+ freq [0 300]))]
    (comb-l (* (white-noise) (env-gen (env-perc 0 0.00001) gate))
            0.1
            delay-time
            0.5)))

;; 2015.12.23
(defsound test option
  (let [gate (impulse 2 [0 1/2])
        fenv (env-gen (envelope [0 0.25 1 8] [0 0.1 0.1]) gate)
        freq (* (dt 1/2 [400 50 200 800 1500]) fenv)
        delay-time (/ 1 (+ freq [0 100]))]
    (comb-l (* (white-noise) (env-gen (env-perc 0 0.00001) gate))
            0.1
            delay-time
            0.5)))

;; 2015.12.25
(defsound test option
  (let [gate (< 0.4
                (m-map lf-pulse (/ [1 3 7 11] 5)))]
    (* (sin-osc [440 660]) (env-gen (env-perc 0.05 0.5) gate))))

;; 2015.12.25
(defsound test option
  (let [gate (< (lf-saw:kr 3) (lf-saw:kr (dt:kr 2 [7 7 2.5 9 1.1])))
        gate2 (impulse 1/2)]
    (* (sin-osc [440 800])
       [(env-gen (env-perc 0.05 0.2) gate)
        (env-gen (env-perc 0.05 1) gate2)])))

;; 2015.12.26 (like burn)
(defsound test option
  (let [gate (impulse 1)
        freq (dt [1 1/2 1/2] [300 1200 800 3000])]
    (switch (lf-pulse 1/4)
            (lf-noise1 freq)
            (lf-noise0 freq))))

;; 2015.12.26
(defsound test option
  (let [gate (impulse 1)
        freq (dq gate [440 800 400 500 1200])
        env (env-gen (envelope [0 0 1] [0 0.4]) gate)
        fenv (env-gen (envelope [0 1 1 1.3 0] [0 0.8 0.05 0.01]) gate)]
    (lpf (m-map #(* %2 env (rg-lin (sin-osc 1.2 (rand 2)) 0.6 1)
                    (lf-tri (* freq fenv % (vib 0.9))))
                [0.3 1 2.8]
                [0.5 1 0.1])
         (* 3 freq))))

;; 2015.12.26
(defsound test option
  (let [freq (rg-lin (lf-pulse (dt:kr 1 [8 1])) (dt:kr 1 [700 2100]) 400)
        env (env-gen (envelope [1 1 0] [1.5 0.5]) :action FREE)]
    (* env (saw (* (rg-lin (sin-osc 54.0) 0.9 1.05) freq [0.99 1.01])))))

;; 2015.12.26(like duck screaming)
(defsound test option
  (let [gate (impulse 1/2 [0 1/3])
        fenv (env-gen (envelope [3 1.2 1 0] [0.24 1 0.2]) gate)
        freq (dq gate [440 608 1030])
        env (env-gen (envelope [0 1 1 0] [0.14 1.2 0.3]) gate)]
    (* env (saw (* fenv (rg-lin (sin-osc 30) 0.3 2) freq)))))

;; 2015.12.27
(defsound test option
  (let [gate (impulse 4 [0 1/16])
        step (demand gate 0 (drand (range 0 1 0.1) INF))
        env (env-gen (envelope [0 1 0] [0 2] EXP) (stepper gate 0 0 7))
        freq (lin-lin step 0 1 300 1800)
        rq (lin-lin step 0 1 0.1 0.3)]
    (ringz (m-map #(rlpf (* 1/4 %2 env (white-noise)) (* freq %) rq)
                  [1 3/2 5/3 8/3 1/2 19/3]
                  [1 1/4 1/2 1/4 1/2 1/20])
           (rg-exp (lf-saw 2) 100 10000)
           0.005)))

;; 2015.12.27
(defsound test option
  (let [am (latch:kr (lf-saw:kr 1/10 -1) (impulse 4))
        ratio (rg-lin (lf-pulse 12) 0.8 1)
        freq (* (lin-exp am -1 1 300 10000)
                ratio)]
    (sin-osc freq)))

;; 2015.12.27
(defsound test option
  (let [gate (impulse 4 [0 1/2])
        freq (lin-lin (demand gate 0 (drand (range 100 250 10) INF)))
        fenv (env-gen (envelope [0 2 1] [0 0.02]) gate)
        env (env-gen (envelope [0 1 1 0] [0.05 0.1 0.1]) gate)]
    (rlpf (* env (lf-tri (* fenv freq))) freq env)))

;; 2015.12.27
(defsound test option
  (vb-sin:ar [(dt (/ 1 [1 1 1 2 2 2 8 8 8 8]) (n-range 100 1000 10))
              (dt 1/4 (n-range 100 1000 10))] 0.2 10))
;; 2015.12.27
(letfn [[f [n]
         (when (< n 30)
           ((synth "audition-synth"
                   (out 0
                        (hold (* 1/10 (line 0 1 3) (sin-osc (ranged-rand 100 1000)))
                              20 3 "done" FREE))))
           (apply-by (+ 1000 (now)) f [(inc n)]))]]
  (f 0))

;; 2015.12.29
(defsound test option-d
  (let [freq (* dr (dt [1 1 1/2 1/2] [100 50 70 80]))
        freq (+ freq (rg-lin (sin-osc 1/3) 0 10))]
    (+ (saw freq)
       (saw (+ freq 1)))))

;; 2015.12.29
(defsound test option-d
  (let [gate (impulse 2)
        freq (* dr (dq gate [100 80 100 70]))
        env (env-gen (envelope [0 1 1 0] [0.1 0.3 0.2]) gate)
        d (a2k (rg-lin (sin-osc 1/4) 1 10))]
    (* env (+ (lf-tri freq)
              (lf-pulse (+ freq d))))))

;; 2015.12.29 (strange)
(defsound test option-d
  (let [freq (* dr 500)
        base-freq 60
        pm (reduce (fn [acc _] (rg-lin (sin-osc acc) 0 base-freq))
                   (sin-osc base-freq)
                   (range 4))
        pm (lin-lin pm -1 1 0 (* 2 Math/PI))]
    (- (sin-osc freq pm)
       (sin-osc freq))))

;; 2016.01.03
(defsound test option-d
  (let [freq (* dr (rg-exp (lf-saw 1/8 -1) 10 4000))]
    (* (brown-noise) (lf-noise0 freq))))

;; 2016.01.04
(defsound test option-d
  (let [gate (impulse (dt:kr 2 [4 8]))
        freq (* dr (dq gate [440 480 280 640]))
        width (env-gen (envelope [0.05 0.05 1] [6 2]))]
    (switch (lf-pulse (a2k (rg-lin (lf-noise0 2) 5 12)) 0 width)
            (sin-osc freq)
            (saw freq))))

;; 2016.01.05
(defsound test option-d
  (let [gate (impulse 1/10)
        freq 440]
    (m-map #(* (sin-osc (* dr % freq))
               (env-gen (envelope [0 1 1 0] [% 1 1.5]) gate))
           [1 1.2 2.08 4.1])))

;; 2016.01.07
(defsound test option
  (rlpf (switch (lf-pulse [16 16.1])
                (lf-tri 400)
                (pulse 510))
        (+ 100 (sweep (impulse [1/4 1/6]) [800 2400]))
        (sweep (impulse [3 1.2]) 0.05)))

(defsound test option
  (let [freq (lag (dt 1 [440 420 600 380 800 680 520])
                  0.7)]
    (m-map #(* %2 (lin-lin (vb-sin 1.2) -1 1 0.2 1)
               (lag-ud  (lf-pulse 4 0 0.8) 0.05 0.05)
               (sin-osc (* freq %)))
           [1 5/3 7/3 18/3 1/2]
           [1 1/2 1/2 1/4 1/2])))

;; 2016.01.09
(defsound test option
  (pan2 (white-noise) (lf-pulse 8)))
(defsound test option
  (pan2 (sin-osc 440) (lf-pulse 8)))
(defsound test option
  (pan2 (sin-osc 440) (lf-tri 8)))
(defsound test option
  (pan2 (sin-osc 440) (rg-lin (lf-tri 8) 0.2 0.8)))
(defsound test option
  (pan2 (white-noise) (rg-lin (lf-tri 8) 0.2 0.8)))

;; 2016.01.09
(defsound test option-d
  (let [gate (impulse 4)
        attack (dq gate [0.08 0.01])
        dur  (dq gate [0.3 0.1 0.1])]
    (* (env-gen (env-perc attack dur) gate) (white-noise))))

;; 2016.01.09
(defsound test option
  (rlpf (lf-noise0 400) 800 (x-line 0.5 0.001 10)))
(defsound test option
  (moog-ff (lf-noise0 400) 800 (line 1 4 10)))
(defsound test option
  (* (lf-saw (mouse-x 10 200)) (cubed (white-noise))))
(defsound test option
  (let [freq (mouse-x 10 400)]
    (latch:ar (sin-osc freq) (impulse freq))))

;; 2015.01.11
(defsound test option-d
  (m-map #(let [snd (saw 25)]
            (ringz snd % 0.1))
         (repeatedly 10 #(rg-exp (lf-noise0 1/2) 500 8000))))

(defsound test option-d
  (m-map #(let [snd (saw (rg-lin (sin-osc 10) 10 100))]
            (ringz snd % 0.1))
         (repeatedly 10 #(rg-exp (lf-noise0 1/2) 500 8000))))

(defsound test option-d
  (m-map #(let [snd (dust 120)]
            (ringz snd % 0.1))
         (repeatedly 10 #(rg-exp (lf-noise0 1/2) 500 8000))))

;; (like hard metal)
(defsound test option-d
  (m-map #(let [freq (x-line:kr 1 200 10)
                snd (latch:ar (lf-pulse 12) (impulse freq))]
            (ringz snd % 0.1))
         (repeatedly 10 #(rg-exp (lf-noise0 1/2) 500 8000))))
;; 2016.01.11
(defsound test option-d
  (let [freq (env-gen (envelope [10000 100] [3]) :action FREE)]
    (rlpf (saw (* dr 50)) freq 0.1)))

;; 2016.01.12
(defsound test option
  (let [freq (line 220 300 10)]
    (m-map #(sin-osc (* % (rg-lin (sin-osc %2) 0.8 1) freq))
           (take 20 (iterate #(* (+ 1 (rand 0.4)) %) 1))
           (repeatedly 20 #(ranged-rand 0.01 1/5)))))

;; 2016.01.13
(defsound test option
  (map
   (fn [max-freq]
     (m-map
      (fn [phase]
        (let [arr (sort (repeatedly 10 #(choose (n-range 0 1 10))))
              dur-arr (repeatedly (dec (count arr))
                                  #(choose [0.01 0.05 0.03]))
              gate (impulse (/ 1 (sum dur-arr)) phase)
              freq (env-gen (envelope arr dur-arr)
                            gate)
              freq (lin-lin freq 0 1 200 max-freq)]
          (* (lf-tri 1/4 phase) (sin-osc freq))))
      (n-range 0 1 6)))
   [1200 6000]))

;; 2016.01.15
(defsound test option-d
  (free-verb
   (* (rlpf (white-noise) (rg-lin (sin-osc 1/2) 200 2000))
      (lf-pulse 4 0.4))
   (line 0 1 5) (line 0.1 10 10)))

;; 2016.01.15
(defsound test option-d
  (let [freq (latch:ar (x-line 6000 100 10) (impulse 10))
        d 0.14
        mod-freq (/ freq (rg-lin (lf-tri 1/3) (- 2 d) (+ 2 d)))]
    (pm-osc freq mod-freq 5)))

;; 2016.01.18
(defsound test option-d
  (let [freq (line 5000 100 0.5)
        index (rg-lin (lf-saw 16) 1 8)] (pm-osc freq (/ freq 2) index)))

;; 2016.01.18
(defsound test option-d
  (let [freq (x-line 8000 100 1)
        freq (* freq dr (rg-lin (lf-pulse 16) 1 1.3))]
    (ringz (brown-noise) freq 0.02)))

;; 2016.01.19
(defsound test option-d
  (m-map #(let [gate (impulse 1 %)
                freq (+ (dq gate [440 600 930 800])
                        (* dr 400 %))] (* (sin-osc freq)
                (env-gen (env-perc 0.05 0.5) gate)))
         (n-range 1/2 1 6)))

;; 2016.01.20
(defsound test option-d
  (* 48 (m-map #(bpf (latch:ar (lf-noise0 32)
                               (impulse 32 %2)) % %2)
               [100 100 300 830 4800]
               [0.1 0.1 0.01 0.05 0.3])))

;; 2016.01.21
(defsound test option-d
  (let [freq2 (dt:kr 1/2 [64 15 258 30])
        freq (dt 2 [440 600 560 1200])]
    (* (switch (lf-tri 1/3) (lf-pulse freq2) (lf-saw freq2))
       (sin-osc freq))))

(def fib-seq (letfn [(fib-step [[a b]] [b (+ a b)])]
               (map first (iterate fib-step [0 1]))))
(defn fib  [n] (nth fib-seq (+ n 1)))
(defn fibs [n] (take (+ n 1) (drop 1 fib-seq)))

;; 2016.01.21
(defsound test option
  (let [gate (impulse 2)
        fenv (env-gen (envelope [600 600 1200] [0.03 0]) gate)]
    (tanh (* 10 (rlpf (white-noise) fenv 0.1)
             (env-gen (env-perc 0.005 0.225) gate)))))

;; 2016.02.01
(defsound test option
  (let [gate (impulse 1)
        freq [(dq gate [440 440 330 660
                        440 440 330 880])
              (dq gate [442 442 330 880
                        444 444 330 882])]]
    (+ (* (sin-osc freq)
          (lag-ud (lf-saw 4) 0.02 0.02) (lf-pulse 1 1/4))
       (switch (demand gate 0 (dseq [1 1 0 1] INF))
               (saw (env-gen (envelope [0 10 10000] [0 1]) gate))
               (pulse (env-gen (envelope [0 10000 10] [0 1]) gate))))))

;; 2016.02.02
(defsound test option
  (let [p-freq (dt:kr 1 [16 8 2 2 1/2])
        gate (impulse p-freq)
        env (env-gen (env-perc 0.01 (/ 1 p-freq)) gate)
        freq (dq gate [400 420])]
    (* (pulse freq) env)))

;; 2016.02.02
(defsound test option-d
  (let [arr (take-while #(< % 20000) (iterate #(* 1.1 %) 200))
        freq (* dr (dt 1/32 arr))]
    (sin-osc freq)))
(defsound test option-d
  (let [arr (take-while #(< % 20000) (iterate #(* 1.03 %) 200))
        freq (* dr (dt 1/32 arr))]
    (sin-osc freq)))
(defsound test option-d
  (let [arr (take-while #(< % 20000) (iterate #(* 1.2 %) 200))
        freq (* dr (dt 1/32 arr))]
    (sin-osc freq)))

;; 2016.02.03
(defsound test option-d
  (m-map #(let [freq (env-gen (envelope [300 6000 300  6000 300 2000 10]
                                        [10/8 0   10/8 10/4 10/4 10/4])
                              (impulse 1/10))
                l (line 0.3 8 10)
                freq2 (squared (rg-lin (sin-osc l %) 0.9 1.05))]
            (pulse (* dr freq freq2) (lf-noise2 l)))
         (repeatedly 10 #(ranged-rand 0 3))))

;; 2016.02.03
(defsound test option-d
  (let [freq (* dr (env-gen (envelope [200 1000 1000 2000 2000 8000]
                                 (repeat 5 2)) :action FREE))
        vol (env-gen (envelope [0 1 0 1] [6 2 3]) :action FREE)]
    (* vol (select (stepper (impulse 8) 0 0 4)
                   [(sin-osc [freq (* 2/3 freq)])
                    (lf-tri freq)
                    (pulse freq)
                    (saw freq)]))))

;; 2016.02.04
(defsound test option-d
  (let [freq (* dr (stepper (impulse 4) 0 100 1000 400))
        freq2 (lag-ud (rg-lin (lf-pulse 16) 0 180) 0.1 0.05)]
    (sin-osc (- 1000 freq freq2))))
;; 2016.02.04
(defsound test option-d
  (m-map #(let [freq (* dr (stepper (impulse 4) 0 100 (* 1000 %) 400))
                freq2 (lag-ud (rg-lin (lf-pulse 16) 0 180) 0.01 0.1)]
            (* (sin-osc (- 1000 freq freq2))
               (step %)))
         (range 0 10)))

;; 2016.02.05
(defsound test option-d
  (let [rythm (mk-rythm 0.1 [1 1/4 1 1/4 1/4])
        power (dq rythm [1 1 1/4 1 1/8])]
    (+ (* (saw (* dr 440)) power rythm)
       (* (sin-osc 1/7) (rlpf (pulse (* dr 100) (rg-lin (sin-osc 3.3) 0.5 1))
                              (rg-exp (sin-osc 440) 100 1000))
          (lf-pulse power)))))

;; 2016.02.05
(letfn [[f [n]
         (when (< n (* 5 1000 1/30))
           ((synth "audition-synth"
             (out 0
                  (hold (* 1/10 (line 0 1 3) (sin-osc (exp-rand 100 10000)))
                    10 3 :done FREE))))
           (apply-by (+ 30 (now)) f [(inc n)]))]]
  (f 0))

;; 2016.02.06
(defsound test option-d
  (m-map (fn [phase base-freq]
           (let [freq (* dr (rg-lin (sin-osc 1/3) base-freq (* 1.1 base-freq)))
                 env (m-map #(lf-pulse % phase) [1.2 1.8 1.4])]
             (* (sin-osc freq)
                env)))
         (n-range 0 1 8)
         (n-range 400 900 8)))

(defsound test option-d
  (let [gate (impulse 1)
        max-freq (dq gate [10000 3000 8000 1800 9000])]
    (m-map #(let [freq (* dr (env-gen (envelope [0 500 max-freq] [0 %] EXP)
                                 gate))]
              (sin-osc freq))
           (n-range 0 1 8))))

;; 2016.02.06
(defsound test option
  (let [gate (impulse [1/2 1.5])
        freq (dq gate [400 800 600 550 530])
        env (env-gen (envelope [0 1 1 0] [0.03 0.7 0.1]) gate)
        fenv (env-gen (envelope [0 6 3] [0 0.08]) gate)]
    (* 1/4 (+ (sin-osc (* (rg-lin (sin-osc 1) 0.99 1) freq))
              (rlpf (pulse (* [2 2/3] freq) (rg-lin (lf-noise2 8) 0.2 1))
                    (* fenv freq)))
       env)))

;; 2016.02.07
(defsound test option
  (let [gate (impulse 1/3)
        freq (dq gate [300 200 350 700])
        ratio (dq gate [3 2 8 5])]
    (sin-osc [(env-gen (envelope [0 freq (* ratio freq)] [0 3]) gate)
              (env-gen (envelope [0 (* ratio freq) freq] [0 3]) gate)])))

;; 2016.02.07
(let [freqs (repeatedly 2
             (fn [] (->>
                     (repeatedly 30 #(choose [1/2 3/2 4/3]))
                     (multiples 400)
                     (filter #(and (< 100 %) (< % 1000))))))]
  (defsound test option
    (let [gate (impulse (dt:kr [4 4 2] [4 2 6]))
          vol (dq gate [1 1/2 2/3 1/2])
          freqs (map #(dq gate %) freqs)]
      (* vol (sin-osc freqs)))))

;; 2016.02.08 (like timing)
(defsound test option
  (let [gate-freq (a2k (rg-lin (lf-saw 1/4) 1/4 8))
        gate (impulse gate-freq)
        fenv (env-gen (envelope [0 3 1] [0 0.02]) gate)
        max-freq [(env-gen (envelope [300 300 800 800 1200] [3 1 3 3]))
                  (env-gen (envelope [300 300 1200 1400 2400] [3 1 3 3]))]
        freq (* (rg-lin (lf-noise0 gate-freq) 100 max-freq) fenv)
        env (env-gen (env-perc 0.02 0.08) gate)]
    (* 8 env (bpf (white-noise) freq 0.02))))

;; 2016.02.08
(defsound test option-d
  (let [max-clip (rg-lin (sin-osc (x-line 256 2560 30) 1) -0.9 1)
        freq-freq (m-map #(switch (lf-pulse 1 3/4)
                                  (lf-saw %) (lf-pulse %))
                         [1 1/8 1/7 2.4 2.9])
        freq (lin-lin freq-freq 0 1 440 2200)
        freq (* freq dr (env-gen (envelope [1 1 4] [5 25]) :action FREE))]
    (hpf (clip:ar (sin-osc freq) -1 max-clip) 800)))

;; 2016.02.11 (like air-plane)
(defsound test option-d
  (let [dur 30
        freq (* dr (x-line 1000 10000 dur))
        env (env-gen (envelope [0 1 1 0] [1 (- dur 2) 1]) :action FREE)]
    (* env (m-map #(+ (rlpf (pink-noise) (* % freq) %2)
                      (* 1/4 (pulse (* 3 freq) (lf-noise0 8))))
                  [1 3/2 18/5]
                  [0.1 0.03 0.01]))))

;; 2016.02.11
(defsound test option
  (let [gate (lf-pulse 32 0 1/8)
        freq (dq gate [4000 5200 3800])]
    (* (lf-pulse 1) gate (pulse freq))))

;; 2016.02.11
(defsound test option
  (let [freq (latch (line:kr 1500 2000 30) (impulse 1))
        speed (x-line:kr 3 12 30)]
    (* (lf-pulse speed)
       (m-map #(* %2 (pulse (* % freq)))
              [1.01 0.98 2/3]
              [1 1 1/4]))))

;; 2016.02.11 (wa-ga-ma-ma)
(defsound test option-d
  (m-map #(let [gate (lf-pulse 1)
                base-freq (- 1 (* gate (rg-lin (sin-osc 1) 0 1)))
                d-freq (lin-lin base-freq 0 1 (* % 1.2) 0)
                freq (* dr (dt 4 [400 900 1800]))]
            (* gate (sin-osc (+ freq d-freq)) (lf-pulse 7)))
         (n-range 250 2000 10 )))

;; 2016.02.11
(defsound test option-d
  (let [pulse-freq (dt:kr 1 (concat (take 11 (cycle [1 1 1 8]))
                                    [18]))
        t (latch (sin-osc:kr 1/2) (impulse pulse-freq))
        speed (x-line:kr 3 12 30)
        gate (lf-pulse speed)
        freq (dq gate [800 780])
        freq (+ freq (* t freq dr 1/3))]
    (* gate
       (m-map #(pulse (* % freq)) [0.99 1.01]))))

;; 2016.02.11
(defsound test option-d
  (let [freq 660
        freq-env (env-gen (envelope [1 1 1.2] [5 55]) :action FREE)
        freq (* dr freq freq-env)
        filter-env (rg-exp (sin-osc 0.22 1) 3 20)
        env (line 0 1 3)]
    (* env (lpf (m-map #(* (sin-osc 1/21 %1)
                           (pulse (* %2 freq) 0.5))
                       (n-range 0 6 4)
                       [1 3/2 7 3])
                (* filter-env freq)))))
;; 2016.02.13
(defsound test option
  (let [gate (impulse (env-gen (envelope [1 1 10 1/2] [3 4 3])))
        freq (dq gate [400 650 500])
        d-freq (* 10000 (lag gate 1) (lf-noise2 32))]
    (pulse (+ freq d-freq))))

;; 2016.02.13
(defsound test option
  (let [gate (impulse [(dt:kr 3 [1 2 12 1])
                       1/2])
        freq (dq gate (multiples 440 [9/4 6/11 9/8 9/8 9/8 1/2 3/2]))
        fenv (env-gen (envelope [0 2 0.7 1] [0 0.03 0.2]) gate)
        freq (* fenv freq [1 1/2])]
    (+ (sin-osc freq)
       (m-map #(* (/ 1 %) (sin-osc (* %2 4 freq)))
              (range 1 10)
              (map #(* (rg-lin (sin-osc %) 0.97 1) %)
                   (range 1 10))))))

;; 2016.02.13 (motor? need more dig)
(defsound test option
  (comb-l (white-noise)
          0.03
          (rg-lin (sin-osc [1/8 1.2]) 0.01 0.03)
          1))

;; 2016.02.14
(defsound test option-d
  (let [ratio (dt [2 1 1/2 1/2] [1 1.5 3 4])]
    (m-map #(let [gate (impulse 1 %)
                  fenv (env-gen (envelope [0 1 1 1.5] [0 0.1 0]) gate)
                  env (env-gen (env-perc 0.05 0.5) gate)]
              (* 3 env (sin-osc (* fenv dr %2 ratio))))
           (n-range 1/4 2 15)
           (multiples 300 (take 15 (cycle [4/3 3/5 5/4 6/5]))))))

;; 2016.02.15
(defsound test option-d
  (let [d (rg-lin (lf-saw 1/3) 0.01 0.03)
        gate (impulse 8)
        freq (dq gate
                 (multiples 440
                            (flatten [(take 13 (cycle [13/10 8/9]))
                                      (take 6 (cycle [5/9 8/7]))
                                      (take 9 (repeat 11/10))
                                      (take 13 (cycle [13/10 8/9]))
                                      ])))
        fenv (env-gen (envelope [0 2 0.9 1] [0 0.01 0.2]) gate)]
    (rlpf (pulse (* freq dr (map #(% 1 d) [- +]))) (* 2 freq fenv))))

;; 2016.02.15
(defsound test option-d
  (m-map #(let [gate (* (lf-pulse 1 %) (impulse %2 %))]
            (* (bpf (white-noise) (* dr %4)) (env-gen (env-perc 0.01 %3) gate)))
         [1/3 0 1/2 0.55]
         [2 6 12 32]
         [0.4 0.1 0.005 0.0001]
         [1800 4000 800 300]))

;; 2016.02.16 (like perc)
(defsound test option-d
  (+ (let [gate (* (lf-pulse 1) (impulse 12))
           freq (* dr (env-gen (envelope [0 2800 680] [0 0.01]) gate))
           env (env-gen (env-perc 0.00001 0.02) gate)]
       (* env (bpf (white-noise) freq)))
     (let [freq (dt 4 [1200 2400])]
       (* (lf-pulse 1/2 1/2 1/4) (lf-pulse 16 0 1/8) (lf-tri freq)))
     (* 1/8
        (env-gen (env-perc 0.02 1) (impulse (dt:kr 1 [1 1 1 4])))
        (rlpf (white-noise) (rg-lin (white-noise) 3500 4000) 0.01))))

;; 2016.02.17
(defsound test option
  (let [gate (impulse 1)
        fenv (env-gen (envelope [0 690 100] [0 0.005]) gate)]
    (* 100 (bpf (white-noise) fenv (rg-lin (white-noise) 0.01 0.5))
       (env-gen (env-perc 0.001 0.04) gate))))

;; 2016.02.17
(defsound test option
  (let [freq [200 480]
        gate (impulse [1 3.6] [0 1/2])
        fenv (env-gen (envelope [0 4 1] [0 0.2]) gate)]
    (* (latch:ar (sin-osc (* fenv freq))
                 (impulse (rg-lin (sin-osc:kr [0.8 0.3]) 100 [400 500])))
       (map #(env-gen (env-perc %3 %2) %1)
            gate
            [0.5 0.05]
            [0.01 0.1]))))

;; 2016.02.17
(defsound test option
  (let [freq (dt 1 [-4 2 0.5 0.1 8])]
    (latch:ar (sin-osc 440)
              (impulse:ar (rg-lin (lf-saw freq 1) 100 2000)))))

;; 2016.02.17
(defsound test option-d
  (ringz (latch:ar (saw (* dr (rg-lin (sin-osc 1/7) 200 800)))
                   (impulse:ar (rg-lin (sin-osc 3.2) 1190 1200)))
         (rg-exp (sin-osc 1/4) 100 4000) 0.001))

;; 2016.02.18
(defsound test option-d
  (let [gate (impulse 1/2)
        max-freq (* dr (dq gate [3000 800]))
        density (dq gate [25 80 120])
        freq (lin-lin (lag:kr (dust:kr density) 0.01) 0 1 0 max-freq)]
    (tanh (switch (lf-pulse 1/3)
                  (saw freq)
                  (rlpf (white-noise) freq 0.1)))))

;; 2016.02.18
(defsound test option
  (let [arr [2/3 7/8 7/8 7/8 9/4 5/8 7/4]
        freqs [(dt [1 1/4 1/4 1/4] (multiples 720 arr))
               (dt [1 1/4 1/2] (multiples 440 arr))]]
    (* (sin-osc (lag-ud freqs 0.03 0.08))
       (clip (+ (* (lf-pulse 1.3 1/8) (line:kr 0 1 10))
                (env-gen (env-perc 0.08 0.7) (impulse 1)))
             0 1))))

;; 2016.02.20
(defsound test option-d
  (let [freq (* dr (+ 500
                 (env-gen (envelope [0 1000 0] [0.1 0.04])
                          (impulse (dt:kr [1 1 1/8] [1 1 8])))
                 (env-gen (envelope [0 -300 0] [0.3 0.1])
                          (impulse 1/3 1/2))))]
    (sin-osc freq)))

;; 2016.02.21
(defsound test option-d
  (* (ringz (latch:ar (saw (* dr (rg-lin (sin-osc 1/7) 200 800)))
                      (impulse:ar (rg-lin (sin-osc 3.2) 1190 1200)))
            (rg-exp (sin-osc 1/4) 100 4000) 0.001)
     (switch (lf-pulse 1/6 1/8)
             (env-gen (env-perc 0.05 0.3) (impulse 2))
             1)))

;; 2016.02.21
(defsound test option-d
  (let [a (lf-saw 1 1)
        freq (* dr (latch:ar (lin-exp a -1 1 100 10000)
                          (impulse 18))
                (rg-lin (lf-pulse 8) 0.7 1.8))]
    (sin-osc freq)))

;; 2016.02.21(like)
(defsound test option-d
  (let [a (lf-saw 1 1)
        freq (lin-exp a -1 1 200 10000)
        snd (switch (lf-pulse 1/4 0 1/4)
                    (ringz (latch:ar (saw (rg-lin (sin-osc 1/7) 200 800))
                                     (impulse:ar (rg-lin (sin-osc 3.2) 1190 1200)))
                           (rg-exp (sin-osc 1/4) 100 4000) 0.001)
                    (saw (* dr 50)))]
    (rhpf snd freq 0.3)))

;; 2016.02.27
(defsound test option
  (let [df (lag-ud (rg-lin (lf-pulse 8) 300 500) 0.005 0.07)
        freq [(dq (impulse 1 0) [200 700 1800])
              (dq (impulse 1 1/8) [280 650 2800])]
        vib (rg-lin (sin-osc 1.8) 0 25)]
    (* (rg-lin (sin-osc 3.2) 0.8 1) (sin-osc (+ freq df vib)))))

;; 2016.02.27
(defsound test option
  (splay (map #(let [gate (impulse 1 %)
               freq (* %2 (dq gate [1 1 1.5]))
               freq (* freq (rg-lin (lf-pulse (line:kr 4 10 20) %)
                                    1 1.1))]
           (sin-osc freq))
        (n-range 0.1 1 10)
        (n-range 400 2000 10))))

;; 2016.02.28
(defsound test option
  (splay (map #(let [gate (impulse 1/10 %)
               freq %2
               freq (env-gen (envelope [freq freq
                                        (* 4 freq)
                                        0] [5 0.18 0.02]) gate)
               freq (* freq (rg-lin (sin-osc 1.3 (rand 6)) 1 1.04))
               env (env-gen (envelope [0 1 1 0] [3 2 0.3]) gate)]
           (* env (sin-osc freq)))
        (n-range 0 1 10)
        (multiples 400 [4/3 4/3 11/5 3/4 3/4 7/8]))))
;; 2016.02.28
(defsound test option
  (splay (map #(let [gate (impulse 1/13 %)]
           (* %3 (env-gen (env-perc 5 8) gate) (sin-osc %2)))
        [1 7/8 6/8 5/8 4/8]
        [1760 220 440 880 3520]
        [1 1/2 1/8 1/16 1/128])))

;; 2016.02.28
(defsound test option
  (splay (map #(* (env-gen (env-perc 0.00001 0.000001) (dust:kr 10))
            (sin-osc (rg-lin (sin-osc 1/10) % %2)))
        [300 400 800 800]
        [500 1000 1800 3800])))

;; 2016.02.28
(defsound test option
  (let [gate (impulse 1)
        freq (dq gate [1000 800 508 750])
        freq (* freq (env-gen (envelope [0 3 0.8 1] [0 0.01 0.02]) gate))
        snd (* (white-noise) (env-gen (env-perc 0.01 0.01) gate))
        snd (comb-l snd 0.1 (/ 1 freq) 0.8)]
    (tanh (g-verb snd 1 1))))

;; 2016.02.28
(defsound test option
  (let [freq (latch:ar  (x-line [8000 440]
                                [2220 8000] 2)
                        (impulse 9 [0 1/4]))
        freq (* freq (rg-lin (lf-pulse 6 [0 1/2]) 1 1.5))]
    (sin-osc freq)))

;; 2016.02.29 (broken)
(defsound test option-d
  (let [gate (impulse 1)
        freq (* dr (lin-lin (m-map lf-pulse [1/2 1/3 1/9]) 0 1 300 1200))
        freq2 (delay-n:kr freq 0.15 0.15)
        snd (clip:ar (* (pulse freq) (impulse (/ freq2 4)))
                     0 0.8)]
    (switch (lf-pulse 0.23 -1/2)
            snd
            (comb-l snd 0.01 0.01 (rg-lin (sin-osc 7/8) 0.6 0.8)))))

;; 2016.03.01
(defsound test option
  (let [gate (impulse 1)
        freq (dq gate [400 696 396])
        freq2 (+ (rg-lin (lf-pulse 16) 0 50) (* freq 3/2))
        env (env-gen (env-perc 0.05 0.5) gate)]
    (* (sin-osc [freq freq2])
       env)))

;; 2016.03.01(low)
(defsound test option-d
  (let [fenv (env-gen (envelope [0 1.3 1] [0 0.08]) (impulse 1))
        env [(env-gen (envelope [0 1 1 0] [0.01 0.1 0.01]) (impulse 1))
             (env-gen (envelope [0 1 1 0] [0.001 0.01 0.5]) (impulse 1 14/16))]
        snd [(m-map #(sin-osc (* fenv %)) (take 13 (iterate #(* 1.133 %) 20)))
             (m-map #(sin-osc (* fenv %)) (take 10 (iterate #(* 1.11 %) 30)))]]
    (mix (* [1 1/2] 4 env snd))))

;; 2016.03.02
(defsound test option
  (let [gate (impulse (dt:kr 1 [4 1]))
        gate2 (impulse (dt:kr 1 [1 2 4]))
        freq [(dq gate (multiples 220 [4/3 4/3 4/7]))
              (dq gate2 (multiples 330 [4/3 4/3 4/7]))]
        snd (lpf (saw freq) (rg-lin (sin-osc 25.6) (* 2 freq) (* 4 freq)))
        env (env-gen (env-perc 0.2 0.5) [gate gate2])]
    (* env snd)))

;; 2016.03.02(beatiful)
(defsound test option
  (m-map #(let [gate (lf-pulse 1 % 3/4)
                freq (lin-exp (sin-osc 1/10 %2) -1 1 100 10000)]
            (* gate (free-verb (* (rlpf (white-noise) freq 0.01) (env-gen (env-perc 0.05 0.5) gate))
                               0.8 1)))
         (n-range 0 1 10)
         (shuffle (n-range 0 1.5 10))))

;; 2016.03.02
(defsound test option
  (let [gate (impulse [1 2] [0 1/8])
        freq (lag-ud (dq gate (multiples 200 [5/4 3/2 7/8 7/8 7/9 7/11])) 0.13 0.2)]
    (ringz (* gate (white-noise)) freq)))

;; 2016.03.03
(defsound test option
  (let [gate (impulse (dt:kr [1/2 1/2 1/2 1/4] [1.5 3]))
        freq (dq gate (multiples 800 [4/5 4/5 4/5 4/5 3/2 6/5]))
        freq2 (* freq (rg-lin (lf-pulse 1/2 0 1/5) 3/2 1.6))]
    (sin-osc [freq freq2])))

;; 2016.03.03
(defsound test option
  (let [gate (lf-pulse [1 (dt:kr 4 [2 3])])
        freq (m-map #(dq % %2)
                    gate
                    [[1000 500 800 700]
                     [750.0 1500.0 1050.0 1200.0]])]
    (* (rlpf (white-noise) freq (rg-exp (sin-osc 4) 0.001 0.3))
       gate)))

;; 2016.03.03
(defsound test option
  (let [gate (impulse 2 [0 1/2])
        freq (dq gate (multiples 400 [3/2 3/2 3/2 4/7 4/7 4/7]))
        fenv (env-gen (envelope [0 3000 freq] [0 0.1]) gate)
        fenv2 (env-gen (envelope [0 4000 200 freq] [0 0.2 0.1] EXP) gate)]
    (rlpf (saw fenv) fenv2 0.3)))

;; 2016.03.03
(defsound test option
  (let [freq (lag (lin-exp (m-map lf-saw [1/3 1/8 1/6 1.8]) 0 1
                           [400 800]
                           [1800 300]) 0.3)
        snd (sin-osc (+ freq (rg-lin (lf-noise1 [3 3]) 0 10)))]
    (comb-n snd 0.3 0.3 1.5)))

;; 2016.03.03
(defsound test option
  (let [gate (lf-pulse 1 0 5/8)
        fenv (env-gen (envelope [0 8000 100] [0 4] EXP) (stepper gate 0 0 4))]
    (+ (* gate (saw [(* 1.01 fenv)
                     (* 0.99 fenv)]) (env-gen (env-perc 0.8 0) gate) (lf-pulse 9))
       (* (- 1 gate) (dust 20)))))

;; 2016.03.03(low)
(demo 10 (let [gate (lf-pulse 2 0 1/12)
               freq (dq gate [80 80 80 80 50 50 120 140])
               fenv (env-gen (envelope [0 2 1] [0 0.008]) gate)]
           (pan2 (* 2 gate (sin-osc (* fenv freq))))))

;; 2016.03.03 (low)
(defsound test option-d
  (let [gate (impulse 2)
        freq (* dr (dq gate [40 40 40 40 35 35 50 55]))
        fenv (env-gen (envelope [0 2 1] [0 0.01]) gate)]
    (* 2 (m-map #(sin-osc (* % freq fenv)) (range 1 3))
       (env-gen (env-perc 0.05 0.2) gate))))

;; 2016.03.04 (minzoku)
(defsound test option-d
  (let [gate (switch (lf-pulse 1/4)
                     (pattern (impulse 16) [1 0 0 0 1 0])
                     (pattern (impulse 16) [1 0 1 0 1 1 1 1]))
        fenv (env-gen (envelope [0 8 1 1 2/3] [0 0.001 1/32]) gate)]
    (* (bpf (white-noise) (* fenv 300)) gate)))

;; 2016.03.05
(defsound test option
  (let [freq (env-gen (envelope [0 100 10000] [0 1] EXP) (impulse 1))
        freq2 (latch:ar freq (impulse 8))]
    (switch (lf-pulse (dt:kr 4 [1/2 8 16]))
            (sin-osc [freq freq2])
            (saw [freq freq2]))))

;; 2016.03.06
(defsound test option
  (let [max-freq (rg-lin (sin-osc (x-line 0.1 30 10)) 1000 12000)
        pulse (impulse (a2k (rg-lin (lf-saw 1/8) 6 16)))
        snd (m-map #(sin-osc (latch:ar (* (/ % 20) max-freq
                                          (rg-lin (sin-osc 3.2 (rand))
                                                  0.99 1.01))
                                       pulse))
                   (range 1 20))]
    (ringz snd 1000 0.01)))

;; 2016.03.06
(defsound test option
  (let [freq (lin-exp (+ (lf-saw 8) (lf-tri 0.3) (sin-osc 0.14))
                      -3 3
                      100 2000)
        freq [freq (latch:ar freq (impulse (line:kr 6 16 10)))]]
    (sin-osc freq)))

;; 2016.03.06
(defsound test option-d
  (let [freq (dt 4 [440 500 700])
        gate (impulse 1.5)
        vib (fn [d]
              (lin-lin (* (sin-osc 3.3 (rand 6))
                          (rg-lin (sin-osc 1/2 (rand 6))
                                  0.3 1))
                       -1 1 (- 1 d) (+ 1 d)))]
    (* (m-map #(* (vib 0.3)
                  (sin-osc (* dr (dq gate %) (vib 0.03) freq)))
              (transpose [[1    3/2 7/3]
                          [1    3/2 5/4]
                          [1.01 3/2 5/4]
                          [1    3/2 4/3]
                          [1    7/4 4/3]]))
       (env-gen (env-perc 0.4 0.1) gate))))

;; 2016.03.06 (rythm)
(defsound test option-d
  (m-map #(let [gate (* (impulse 10) (lf-pulse 1 %))]
            (* 100 (bpf (white-noise) (* dr %2) 0.1)
               (env-gen (env-perc 0.001 0.3) gate)))
         (shuffle (n-range 0 1 10))
         [400 670 1330 806 200 150 180]))

;; 2016.03.07
(defsound test option
  (m-map #(rlpf (white-noise)
                (rg-exp (lf-saw 1/3 %) 300 %2)
                (cubed (white-noise)))
         (n-range 0 1 10)
         (n-range 1000 5000 10)))

;; 2016.03.08
(def behind-the-mask (transpose [(chord :F4 :major)
                                 (chord :Db4 :major)
                                 (chord :Eb4 :major)
                                 (chord :C4 :minor)]))

(defsound test option-d
  (let [gate (impulse 6)
        env (env-gen (env-perc 0.05 0.3) gate)]
    (* (m-map #(sin-osc (dq:kr (stepper gate 0 0 6) (midicps %)))
              behind-the-mask)
       env)))

;; 2016.03.08
(defsound test option-d
  (m-map (fn [n freq freq2]
           (let [gate (impulse freq)]
             (* (/ 1 n) (m-map #(sin-osc (* n (lag (midicps (dq gate %)) 0.8)))
                               behind-the-mask)
                (sin-osc freq2))))
         (iterate inc 1)
         [1/3 1/8 1/2 1/10 1/2]
         [2/3 2   1/4 8    1/8]))

;; 2016.03.08 (rythm)
(defsound test option
  (let [delay (rg-lin (lf-saw:kr 2.1) 0 0.2)
        gate (impulse (dt:kr 5 [4 8]))
        gate [gate (delay-n:kr gate 0.2 delay)]
        freq (rg-lin (lf-noise0 1) 800 4800)]
    (* (rlpf (white-noise) freq) (env-gen (env-perc 0.01 0.1) gate))))

;; 2016.03.09
(defsound test option
  (let [gate [(pattern (impulse 8) [1 0 0 0 0 0 1 0 1 0 0 0 0 0])
              (pattern (impulse 8) [0 0 1 0 0 0 0 0 0 1 0 1 0 0])]
        freq (* [1800 400] (env-gen (envelope [0 8 1] [0 0.008]) gate))]
    (* 2 (bpf (white-noise) freq 0.2)
       (env-gen (envelope [0 1 1 0] [0.01 0.02 0]) gate))))

;; 2016.03.10
(defsound test option
  (let [gate (impulse 1)
        timing (env-gen (envelope [2 2 4 8 1 1/4] [6 2 2 8 8]) :action FREE)]
    (m-map #(* (sin-osc (midicps (dq gate %2)))
               (env-gen (env-perc 0.001 0.3) (impulse timing (/ 1 %))))
           (iterate inc 1)
           behind-the-mask)))

;; 2016.03.10
(defsound test option
  (let [gate (impulse (dt:kr 10 [1/10 4 8]))]
    (map (fn [detune]
           (m-map #(* (sin-osc (midicps (+ (dq gate %2) detune)))
                      (env-gen (envelope [0 1 1 0] [3 3 3]) (delay-n:kr gate 2 (/ 2 %))))
                  (iterate inc 1)
                  behind-the-mask))
         [0 0.05])))
;; 2016.03.10
(defsound test option
  (* (bpf (white-noise) (rg-lin (lf-pulse 1/2) 400 [1200 2400]))
     (lf-pulse 8 [0 1/6] 1/8) (lf-pulse 3 0 3/4)))

;; 2016.03.12(rythm)
(defsound test option
  (let [gate (* [1 (lf-pulse 1/2 0 1/8)] (pattern (impulse [8 16]) [1 1 1 1 1 0 0 0]))
        env (env-gen (env-perc 0.001 0.4) gate)
        freq (dq gate [800 1600 700 820 1200])]
    (* (rlpf (brown-noise) freq) env)))

;; 2016.03.12
(defsound test option
  (let [gate (impulse 1)
        snd (m-map #(let [freq (midicps (dq gate %))]
                      (rlpf (white-noise) freq 0.08))
                   behind-the-mask)]
    (* (switch (lf-pulse 0.3) snd (g-verb snd))
       (env-gen (env-perc 0.05 2) gate)
       (env-gen (env-perc 0.001 0.4) (impulse 8)))))

;; 2016.03.14
(defsound test option-d
  (m-map #(let [gate (impulse 1)
                freq (dq gate (midicps %))
                fenv (* freq (env-gen (envelope [0 0 6 2] [0 0.3 0.7]) gate))
                vib (rg-lin (sin-osc 12.1) (env-gen (envelope [0 1 0.5] [0 1]) gate) 1)
                env (env-gen (env-perc 0.05 1) gate)]
            (* (rlpf (pulse freq) fenv)
               vib env))
         behind-the-mask))

;; 2016.03.14
(defsound test option
  (m-map #(let [gate (impulse 1 %2)
                freq (dq gate (midicps %))
                fenv (* freq (env-gen (envelope [0 0 6 2] [0 0.3 0.7]) gate))
                vib (rg-lin (sin-osc 12.1) (env-gen (envelope [0 1 0.5] [0 1]) gate) 1)
                env (env-gen (env-perc 0.05 1) gate)]
            (* (rlpf (pulse freq) fenv)
               vib env))
         behind-the-mask
         [0.3 0.8 0.85]))

;; 2016.03.15
(defsound test option
  (map (fn [osc phase]
         (m-map #(let [gate (impulse (cubed (a2k (rg-exp (lf-saw 1/6 -1) 1/2 4))))
                       freq (dq gate (midicps %))
                       env (env-gen (env-perc 0.0001 0.08) gate)
                       ]
                   (* (rg-lin (sin-osc 4.8 phase) 0.3 1) (osc freq) env))
                behind-the-mask))
       [saw lf-tri]
       [0 1/2]))

;; 2016.03.17
(defsound test option
  (let [num 8]
    (m-map (fn [ratio phase]
             (m-map (fn [freqs] (let [gate (impulse 1/3 phase)]
                                  (* (sin-osc (* ratio (midicps (dq gate freqs))))
                                     (env-gen (env-perc 1 2) gate))))
                    behind-the-mask))
           (shuffle (n-range 1 12 num))
           (n-range 0 3 num))))

;; 2016.03.18
(defsound test option
  (* (sin-osc (rg-exp (sin-osc 1/3) 20 800))
     (rlpf (white-noise)
           (* (rg-exp (sin-osc [(dt 3 [1/8 3/4 1/16]) (line 1 8 12)]) 100 1000)))))

;; 2016.03.18(low)
(defsound test option-d
  (let [gate (impulse 2)
        ratio (dq gate [1 1 1 1 3/4 3/4 7/9 11/13])]
    (apply m-map #(* (sin-osc (* dr ratio %)) (env-gen (env-perc %2 %3) gate))
           (transpose [ [110 0.1 0.2]
                       [150 0.2 0.1]
                       [170 0.01 0.05]
                       [80 0.05 0.3]
                       [60 0.05 0.1]
                       [40 0.05 0.4]]))))

;; 2016.03.19
(defsound test option
  (m-map #(let [gate (impulse 1)
                freq (midicps (dq gate %))
                fenv (env-gen (envelope [0 8 1] [0 0.23]) gate)]
            (* 8 (bpf (white-noise) (* freq fenv) 0.1)
               (env-gen (env-perc 0.3 0.02) gate)))
         behind-the-mask))

;; 2016.03.19
(defsound test option
  (let [gate (impulse 1/2)]
    (m-map (fn [pulse-freq ratio]
             (m-map #(* (sin-osc (* ratio (midicps (dq gate %)))) (lf-pulse pulse-freq))
                    behind-the-mask))
           [4 (dq gate [12 12 12 16 8 18])]
           [1 1.5])))

;; 2016.03.20
(defsound test option-d
  (m-map (fn [env-freq ratio gate-freq]
           (m-map #(* (saw (* dr ratio (midicps (dq (impulse gate-freq) %))))
                      (lf-tri env-freq))
                  behind-the-mask))
         [1/3 1/8 1/12]
         [1 1.5 4]
         [1/3 1/8 1/12]))

;; 2016.03.22
(defsound test option-d
  (let [gate (impulse 1/2)]
    (m-map #(let [freq (midicps (dq gate %))]
              (* (g-verb (clip2 (lpf (sin-osc (+ freq (* 3 freq (sin-osc (* 3 freq)))))
                                     freq) 0.8)
                         0.35 0.1 0.5 0.5 57.6650390625)
                 (env-gen (envelope [0 1 1 0] [0.0001 0.5 0.4]) gate)))
           behind-the-mask)))

;; 2016.03.22
(defsound test option-d
  (let [gate (impulse 1)]
    (m-map #(* (pulse (* (lag-ud (midicps (dq gate %)) %2 %3) %4)
                      (rg-lin (lf-noise0 8) 0.1 0.5))
               (env-gen (envelope [0 1 1 0] [0.0001 0.12 0.28]) gate))
           behind-the-mask
           [0.4 0.3 0.05]
           [0.1 0.01 0.4]
           [1 2 5])))

;; 2016.03.23
(defsound test option
  (let [[a b c] (map #(midicps (dt 1.5 %)) behind-the-mask)]
    (+ (* (lin-lin (* (sin-osc 2.9) (sin-osc 3.2)) -1 1 0.5 1) (lf-tri c))
       (switch (lf-pulse (a2k (rg-exp (sin-osc 4.2) 1 12))) (sin-osc a) (sin-osc b)))))

(defsound test option
  (let [[a b c] (map #(midicps (dt 1.5 %)) behind-the-mask)]
    (+ (* (lin-lin (* (sin-osc 2.9) (sin-osc 3.2)) -1 1 0.5 1) (lf-tri a))
       (switch (lf-pulse (a2k (rg-exp (sin-osc 4.2) 1 12))) (sin-osc c) (sin-osc b)))))

(defsound test option
  (let [[a b c] (map #(midicps (dt 1.5 %)) behind-the-mask)
        d (switch (lf-pulse 1/2) a c)]
    (+ (* (lin-lin (* (sin-osc 2.9) (sin-osc 3.2)) -1 1 0.5 1) (lf-tri d))
       (switch (lf-pulse (a2k (rg-exp (sin-osc 4.2) 1 12))) (sin-osc c) (sin-osc b)))))

;; 2016.03.24
(defsound test option-d
  (let [[a b c] (map #(midicps (dt [2 1/2 1/2 1 1/4 1/4 1/4 1/4 1/4 1/4] %))
                     behind-the-mask)]
    (+ (* (sin-osc 1/3) (sin-osc (* 2 (switch (lf-pulse 8) a b))))
       (* (rg-lin (sin-osc 1/5) 0.5 1) (pulse (switch (lf-pulse 3) a c)))
       (lf-tri (switch (lf-pulse 4) b c)))))

;; 2016.03.25
(defsound test option
  (let [[a b c] (map #(sin-osc (lag (midicps (dt [2 1/2 1/2 1/2] %)) 0.1))
                     behind-the-mask)]
    [ (switch (lf-tri 2/3) a b)
     (switch (lf-tri 1/3) c a)]))

;; 2016.03.26
(defsound test option
  (let [ratio (dt 1/3 [1 1/4 1 3 1 2 1 1 1 8])]
    (m-map (fn [freq]
             (* 5 (switch (m-map lf-pulse [1 1/3 1/7 1/11])
                          (* 1/4 (* (lf-pulse 8 0 1/8) (sin-osc freq)))
                          (bpf (brown-noise) (* ratio freq) 0.05))))
           (map #(midicps (dt 1 %)) behind-the-mask))))

;; 2016.03.27
(defsound test option
  (let [gate (mk-rythm 0.01 [2 2 2 1/2 1/4 1/4 1/4])
        [a b c] (map #(midicps (dq gate %)) behind-the-mask)]
    (+ (* 1/2 (rg-lin (sin-osc 1.3) 0.3 1)
          (m-map #(map (fn [ov] (sin-osc (* ov %))) [1 3 4])
                 [(* c 1.01) (* c 0.99)]))
       (* 1/4 (ringz (* 1/16 (switch (lf-pulse 3)
                                     (lf-tri b)
                                     (saw c)))
                     (* 3 b) 0.1))
       (* 3 (sin-osc (* 1/2 (switch (lf-pulse 3 0 3/4) c b)))
          (env-gen (env-perc 0.05 1) gate)))))

;; 2016.03.27
(defsound test option
  (let [vib-v (line 0 18 16)
        snd (fn [freq]
              (m-map #(* (sin-osc %) (rg-lin (sin-osc (* vib-v %2) (rand)) 0.2 1))
                     (map #(* % freq) (range 1/2 12))
                     (n-range 0 1 12)))]
    (m-map snd (map #(midicps (dt 2 %)) behind-the-mask))))

;; 2016.03.28
(defsound test option
  (m-map (fn [phase] (let [gate-1 (impulse 2.3 phase)
                           gate-2 (impulse 0.8 phase)]
                       (m-map #(let [freq (midicps (dq gate-1 %))]
                                 (* 4 (sin-osc (* (* 10 phase) freq))
                                    (env-gen (env-perc 0.05 2) gate-2)))
                              behind-the-mask)))
         (n-range 0.1 1 5)))

;; 2016.03.29
(defsound test option-d
  (let [gate (dust:kr 6)]
    (m-map #(* (sin-osc (midicps (dq gate %)))
               (env-gen (env-perc 0.005 0.18) gate))
           behind-the-mask)))

;; 2016.03.30
(defsound test option-d
  (m-map #(let [gate (impulse 1/8 %2)
                fenv (env-gen (envelope [0 0 1 1 8] [0 3 4 1]) gate)
                env  (env-gen (envelope [0 1 1 0] [0 7 1]) gate)]
            (* env (sin-osc (* fenv (midicps (dq gate %))))))
         (take 9 (cycle behind-the-mask))
         (cubed (n-range 0 1 9))))

;; 2016.03.31
(defsound test option
  (apply m-map #(* 32 (bpf (white-noise) % %2)
                   (env-gen (env-perc 0.0001 %3) (impulse 1/3 (rand))))
         (transpose (concat (map #(vector % 0.01 0.05) (n-range 100 500 15))
                            (map #(vector % 0.05 3) (n-range 1000 7000 3))
                            (map #(vector % 0.1 0.6) (n-range 10000 30000 5))))))

;; 2016.03.31
(defsound test option-d
  (let [gate (impulse 1/2)]
    (m-map #(* (sin-osc (lag (midicps (dq gate %))
                             %2))
               (env-gen (envelope [0 1 1 0] [0.5 0.8 0.7]) gate))
           behind-the-mask
           [0.01 1.8 1])))

;; 2016.04.01
(defsound test option-d
  (m-map (fn [freqs phase]
           (let [gate (impulse 1 phase)]
             (rlpf (white-noise) (midicps (dq gate freqs))
                   (rg-exp (lf-pulse 0.7 3/5) 0.01 0.3))))
         behind-the-mask
         (n-range 0 1 3)))

;; 2016.04.01
(defsound test option-d
  (m-map #(let [gate (impulse 1/3 %2)
                fenv (env-gen (envelope [0 0.5 1] [0 1]) gate)
                freq (midicps (dq gate %))]
            (* (env-gen (env-perc 1 2) gate) 4
               (rlpf (saw freq) (* freq fenv))))
         (concat behind-the-mask behind-the-mask)
         (repeatedly 6 #(rand))))

;; 2016.04.02
(defsound test option-d
  (m-map #(let [gate (impulse 1 %2)
                freq (midicps (dq gate %))
                env (env-gen (envelope [0 1 1 0] [0.13 0.5 0.3]) gate)
                ratio (stepper gate 1 1 4)]
            (* env (sin-osc (* ratio freq))))
         behind-the-mask
         (n-range 0 1 3)))

;; 2016.04.03
(defsound test option-d
  (m-map #(let [freq (lag (midicps (dt 1 %)) 0.3)
                fenv (env-gen (envelope [0 1 1 3 1 1 0] [0 3.5 0.5 0 3.5 0.5])
                              (impulse 1/8))]
            (* (x-line %4 1 20)
               (rlpf (saw (* fenv freq)) (* freq (rg-lin (lf-tri %3 %2) 1 5)))))
         behind-the-mask
         (n-range 0 1 3)
         [1/5 8 2]
         [1 1/32 1/8]))

;; 2016.04.03
(defsound test option-d
  (ringz (m-map #(let [freq (/ (midicps (dt 1/6 %)) 2)]
                   (m-map pulse [freq
                                 (* freq 2/3)
                                 (* freq 3/4)]))
                behind-the-mask)
         (rg-exp (lf-tri 1/10 1) 100 5000) 0.01))

;; 2016.04.03
(defsound test option-d
  (m-map #(let [freq (midicps (dt [1/4 1/4 1/4 1 1 2] %))]
            (* 1/2 (sin-osc (* freq (sin-osc (/ freq 4))))))
         (concat behind-the-mask (map (fn [x] (map #(+ 36 %) x)) behind-the-mask))))

;; 2016.04.04
(defsound test option-d
  (let [gate (m-map impulse [1/3 4/5 3/4])]
    (m-map #(let [freq (midicps (dq gate %))
                  env (env-gen (env-perc 0.0001 0.5) (delay-n:kr gate 1 %2))]
              (* 6 (bpf (pulse freq (rg-lin (lf-tri 0.6 %2) 0.1 0.5)) (* 5 freq) 0.05)
                 env))
           behind-the-mask
           (n-range 0 1 3))))

;; 2016.04.04
(defsound test option-d
  (m-map #(let [gate (impulse (dt:kr 5 [1 12]))
                freq (midicps (dq gate %))]
            (* 8 (bpf (saw (* freq (sin-osc (* 1/8 freq))))
                      (* 3 freq) (rg-exp (lf-tri 0.1) 0.001 0.2))))
         behind-the-mask))

;; 2016.04.05
(defsound test option
  (m-map #(let [freq (midicps (dt 1 %))]
            (* (clip2 (sin-osc (* freq 2)) (sin-osc 3.3))
               (clip2 (sin-osc (* freq 1/3)) (lf-tri 1.5))
               (clip2 (sin-osc freq) (lf-saw 1/3))))
         behind-the-mask))

;; 2016.04.06
(defsound test option-d
  (let [gate (impulse 1)]
    (m-map #(let [freq (midicps (dq gate %))
                  fenv (env-gen (envelope [0 1/2 1] [0 0.15]) gate)
                  freq (* fenv freq)
                  env (env-gen (envelope [0 1 1 0] [0.1 0.2 0.5]) gate)]
              (* env
                 (m-map (fn [ratio phase vol] (* vol
                                                 (sin-osc (* ratio freq))
                                                 (lf-pulse 32 phase 0.3)))
                        [1/2 1 2 6]
                        [0 1/2 1/3 1/3]
                        [1 1 1 1/2])))
           behind-the-mask)))
;; 2016.04.07
(defsound test option-d
  (let [gate (impulse 1)]
    (ringz (m-map #(let [freq (midicps (dq gate %))]
                     (* (sin-osc freq)
                        (env-gen (env-perc 0.8 0.1) gate)
                        (cubed (lf-tri (rg-lin (lf-tri 1.3) 3 9)))))
                  behind-the-mask)
           (lag (* 4 (dq gate (midicps (first behind-the-mask)))) 1) 0.5)))

;; 2016.04.08
(defsound test option-d
  (let [snd (m-map #(let [freq (midicps (dt 1 %))]
                      (saw (/ freq 2 (rg-lin (sin-osc 3.1) 0.98 1))))
                   behind-the-mask)]
    (m-map #(* 16 (bpf snd (* 3 (midicps (dt 4 %))) 0.1)
               (env-gen (env-perc 0.01 0.08) (dust:kr 8))) behind-the-mask)))

;; 2016.04.08
(defsound test option-d
  (m-map #(let [gate (impulse 1)
                freq (lag (midicps (dq gate %)) 0.3)
                env (env-gen (envelope [0 1 1] [1 1]) gate)]
            (+ (* (sin-osc (* (rg-lin (sin-osc 2.3 (rand)) 0.98 1) freq))
                  (rg-lin (sin-osc 1/3 (* 3 (rand))) 0.2 1)
                  env)
               (* 1/32 (pulse (* (rg-lin (lf-pulse 1/2) 3 6) freq)
                              (rg-lin (sin-osc 4.1) 0.3 0.5))
                  env)))
         behind-the-mask))

;; 2016.04.09
(defsound test option-d
  (let [snd (m-map #(let [freq (midicps (dt 1 %))]
                      (saw freq))
                   behind-the-mask)]
    (m-map #(* 8 (bpf snd (rg-lin (sin-osc 4 (rand 3.14)) (/ % 2) %))
               (env-gen (env-perc 0.01 0.5) (impulse 1/2 %2)))
           (n-range 2000 10000 8)
           (n-range 0 1 8))))


;; 2016.04.09
(defsound test option-d
  (let [gate (impulse 1)]
    (m-map #(let [fenv (env-gen (envelope [0 1.8 0.98 1 0.98] [0 0.01 0.1 0.8]) gate)
                  env (env-gen (envelope [0 1 1 0] [0.000001 0.2 0.7]) gate)
                  freq (* %2 (midicps (dq gate %))
                          (rg-lin (sin-osc 3.2 (rand 3)) (- 1 (* %2 1/100)) 1)
                          fenv)]
              (* %3 (lf-tri freq) env))
           behind-the-mask
           [1 5 7]
           [1 1/4 1/12])))

;; 2016.04.09
(defsound test option-d
  (m-map #(let [freq (midicps (env-gen (envelope % %2)))]
            (* (rg-lin (sin-osc 1.3 (rand 3.14)) 0.8 1)
               (sin-osc (* freq (rg-lin (sin-osc 3.3 (rand 3.14)) 0.98 1)))))
         (map #(mapcat (fn [x] [x x]) %) behind-the-mask)
         (repeatedly 3 (fn [] [3 1/2 2 6 3 4 8]))))

;; 2016.04.09
(defsound test option-d
  (m-map (fn [phrase]
           (let [freq (midicps (dt 1 phrase))]
             (m-map #(* (saw %2) (env-gen (env-perc 0.05 0.5) (impulse 1 %)))
                    (n-range 3/4 1 5)
                    (map #(* % freq) (conj (range 4 8) 1)))))
         behind-the-mask))

;; 2016.04.11
(defsound test option-d
  (m-map (fn [phrase]
           (let [gate (impulse 3/2)
                 freq (midicps (dq gate phrase))]
             (apply m-map #(* 32 %5 (bpf (white-noise) (* freq %) %2)
                              (env-gen (env-perc %3 %4) gate))
                    (transpose [[1 0.04 0.1 0.1 1]
                                [2 0.03 0.01 0.05 1]
                                [2.5 0.01 0.15 0.05 1/2]
                                [4 0.01 0.1 0.1 1/4]
                                [8 0.05 0.13 0.15 1/24]]))))
         behind-the-mask))
;; 2016.04.12
(defsound test option-d
  (m-map (fn [phrase]
           (let [gate (impulse 3/2)
                 freq (midicps (dq gate phrase))]
             (apply m-map #(* 32 %5 (bpf (white-noise) (* freq %) %2)
                              (env-gen (env-perc %3 %4) gate))
                    (transpose [[1 0.01 0.1 0.3 1]
                                [1.5 0.01 0.1 0.5 1/2]
                                [2 0.03 0.01 0.05 1/4]
                                [3 0.01 0.2 0.1 1/4]
                                [9 0.05 0.13 0.25 1/16]]))))
         behind-the-mask))

;; 2016.04.12
(defsound test option-d
  (m-map #(let [freq (midicps (dt 2 %))
                freq (* freq (lin-lin (m-map lf-pulse %2)
                                      0 1 1 4))]
            (sin-osc freq))
         behind-the-mask
         [[1 1/2 1/3 2.3]
          [1/2 1/3]
          [1 1/2 3.8 3.1]]))

;; 2016.04.13
(defsound test option-d
  (m-map #(let [gate (impulse (dt:kr [4 2 1] [1 2.5 4]) %2)
                freq (midicps (dq gate %))
                env  (env-gen (env-perc 0.05 0.5) gate)
                clip-val (env-gen (envelope [0 0.3 0.98] [0 0.07]) gate)]
            (* env (clip2 (sin-osc freq) clip-val)))
         behind-the-mask
         (n-range (- 1 1/8) 1 3)))

;; 2016.04.14
(defsound test option-d
  (m-map #(let [gate (impulse (dt:kr [4 2 1] [1 2.5 4]) %2)
                length (dt:kr [4 2 1] [0.8 0.2 0.01])
                freq (midicps (dq gate %))
                env  (env-gen (env-perc 0.05 length) gate)
                clip-val (env-gen (envelope [0 %3 %4] [0 %5]) gate)]
            (* env (clip2 (sin-osc freq) clip-val)))
         behind-the-mask
         (n-range (- 1 1/8) 1 3)
         [0.1 0.99 1]
         [0.99 0.9 0.8]
         [0.5 0.1 0.5]))

;; 2016.04.15
(defsound test option-d
  (m-map (fn [xs phase]
           (m-map #(let [gate (impulse 1/7 (- %2 phase))]
                     (* (sin-osc (midicps %))
                        (env-gen (envelope [0 1 1 0] [0.1 0.5 3]) gate)))
                  xs
                  (n-range 1/4 1 4)))
         behind-the-mask
         (n-range 0.01 0.04 3)))

;; 2016.04.15
(defsound test option-d
  (m-map (fn [xs phase]
           (m-map #(let [gate (impulse (line:kr 1/2 2 14) (- %2 phase))]
                     (* 2 (sin-osc (midicps %))
                        (env-gen (envelope [0 1 0] [0.01 0.2]) gate)))
                  xs
                  (n-range 0 1 4)))
         behind-the-mask
         (n-range 0.01 0.03 3)))

;; 2016.04.16
(defsound test option-d
  (m-map (fn [xs phase]
           (m-map #(let [gate (impulse 1/3 (- %2 phase))]
                     (* (sin-osc (* %3 (midicps %)))
                        (env-gen (envelope [0 1 1 0] [0.1 0.8 1.5]) gate)))
                  xs
                  (n-range 1/4 1 4)
                  (range 1 5)))
         behind-the-mask
         (n-range 0 1/4 3)))

;; 2016.04.17
(defsound test option-d
  (let [gate (impulse 1.3)]
    (m-map #(let [freq (midicps (dq gate %))]
              (* %3 (sin-osc (* freq %2))
                 (env-gen (env-perc 0.01 0.02)
                          (impulse (a2k (rg-exp (lf-saw 1/8 -1) 4 64))))))
           behind-the-mask
           [1 3 6]
           [1 1/3 1/10])))

;; 2016.04.18
(defsound test option
  (m-map #(let [gate (dust:kr 2)
                dur %2]
            (* (lf-tri (midicps (dt 1 %)))
               (env-gen (env-perc dur 0.01) gate)))
         (concat (map #(+ 24 %) behind-the-mask)
                 (map #(+ 12 %) behind-the-mask) behind-the-mask)
         (concat [0.1 0.05 0.5]
                 [0.2 0.1 0.3][0.5 0.5 0.5])))
;; 2016.04.18
(defsound test option
  (m-map #(let [gate (mk-rythm 0.1 [0.2 0.4 0.2 0.2 0.1 0.8])]
            (* (rlpf (saw (midicps (dt 2/3 %))) (rg-lin (sin-osc 1/3) 800 2000))
               (env-gen (env-perc %2 %3) gate)))
         behind-the-mask
         [0.01 0.1 0.2]
         [0.2 0.05 0.3]))

;; 2016.04.19
(defsound test option-d
  (m-map #(let [gate (demand (impulse (dt:kr 5 [4 12])) 0 (drand [0 0 0 0 1] INF))
                freq (midicps (dt 4 %))
                fenv (env-gen (envelope [0 3 1.5] [0 0.1]) gate)
                rq-env (env-gen (envelope [0.01 0.5 0.3] [0.1 0.2]) gate)]
            (* (rlpf (pulse freq) (* fenv freq) rq-env) (env-gen (env-perc 0.3 0.01) gate)))
         behind-the-mask))

;; 2016.04.19
(defsound test option-d
  (m-map #(let [gate (impulse (a2k (rg-exp (lf-saw 1/2) 1 12)))
                freq (midicps (dt 1 %))]
            (* (sin-osc freq) (lag (lf-saw 1/2)) (env-gen (env-perc 0.05 0.5) gate)))
         behind-the-mask))

;; 2016.04.20
(defsound test option-d
  (m-map #(let [gate (impulse (line:kr 1 4 20) (/ %2 10))
                freq (* %2 (midicps (dq gate %)))
                env (env-gen (env-perc 0.05 0.5) gate)]
            (ringz (* gate (white-noise)) freq (line 0.5 0.1 20)))
         (concat behind-the-mask behind-the-mask)
         (reverse (range 1 7))))

;; 2016.04.21
(defsound test option-d
  (let [gate (impulse 1)
        [a b c] (map #(midicps (dq gate %))  behind-the-mask)
        env (env-gen (env-perc 0.05 0.5) gate)]
    (+ (ringz (* gate (white-noise)) (switch (lf-pulse 4) a b) 1)
       (ringz (* (impulse 4) (white-noise)) (switch (lf-tri 8) b c) 0.3))))

;; 2016.04.21
(defsound test option-d
  (let [gate (impulse 1)
        arr (map #(midicps (dq gate %)) behind-the-mask)]
    (m-map #(let [gate2 (dust:kr (rg-exp (sin-osc:kr 1/4) 0.5 12))
                  freq (* % (select:kr (demand:kr gate2 0 (drand [0 1 2] INF)) arr))]
              (* (/ 2 %) (env-gen (env-perc 0.05 0.3) gate2)
                 (sin-osc freq)))
           (range 1 7))))

;; 2016.04.21
(defsound test option-d
  (let [gate (impulse 1)
        arr (map #(midicps (dq gate %)) behind-the-mask)
        index (lin-lin (m-map lf-pulse [1/11 1/8 1/3 1/14]) 0 1 0 3)]
    (m-map #(let [gate2 (dust:kr (rg-exp (sin-osc:kr 1/4) 0.5 12))
                  freq (* % (select:kr index arr))]
              (* (/ 2 %) (env-gen (env-perc 0.05 0.3) gate2)
                 (sin-osc freq)))
           (range 1 7))))


;; 2016.04.22
(defsound test option-d
  (let [gate (impulse 0.8)
        [a b c] (map #(midicps (dq gate %)) behind-the-mask)
        env (env-gen (envelope [0 1 1 0] [0.04 0.5 0.4]) gate)]
    (* env (apply m-map #(switch (squared (lf-tri %3 %4))
                                 (pulse (* %5 %))
                                 (lf-tri (* %5 %2)))
                  (transpose [[a b 1/3 0 1]
                              [c a 1/6 1/2 2]
                              [c b 1/8 3/4 4]
                              [b c 1/4 1/3 3]
                              [a c 1/2 1/4 1/2]
                              [b a 1/3 1/4 1/4]])))))

;; 2016.04.23
(defsound test option-d
  (m-map #(let [gate (impulse 1 %2)
                freq (midicps (dq gate %))
                snd (sin-osc freq)
                ratio (lin-lin (clip2 (lf-tri 1 1/2) 0.2) -1 1 1 1/4)
                env (env-gen (env-perc 0.05 0.8) gate)]
            (* env (+ (* 1/2 snd) (* (sin-osc (* ratio freq)) snd))))
         behind-the-mask
         (n-range 3/4 1 3)))

;; 2016.04.23
(defsound test option-d
  (m-map #(let [gate (impulse 2 (* (line:kr 0 1 10) %2))
                freq (midicps (dq gate %))
                fenv (env-gen (envelope [0 4 2 1] [0.02 0.03 0.08] :step) gate)
                snd (rlpf (saw freq) (* freq fenv) 0.2)
                env (env-gen (env-perc 0.03 0.6) gate)]
            (* env snd))
         behind-the-mask
         (n-range 0 1 3)))

;; 2016.04.23
(defsound test option-d
  (let [gate (impulse 2/3)]
    (m-map #(let [freq (midicps (dq gate %))
                  vib (env-gen (envelope [0 1 1 0] [0 0.2 0.4]) gate)
                  vib (rg-lin (lf-tri 3.2 (rand)) vib 1)
                  env (env-gen (env-perc 0.4 0.5) gate)]
              (* env vib
                 (rlpf (pink-noise) (* freq vib) 0.01)))
           (concat behind-the-mask
                   (map #(+ 24 %) behind-the-mask)))))

;; 2016.04.24
(defsound test option-d
  (let [gate (impulse 1)]
    (m-map #(let [freq (midicps (dq gate %))
                  freq2 (/ freq (line 32 8 8))
                  env (env-gen (envelope [0 1 0.8 0.3] [0.0001 0.05 0.9]) gate)]
              (* env (sin-osc freq2) (saw freq)))
           behind-the-mask)))

;; 2016.04.24
(defcgen select-c [which {} array {}]
  (:ar (let [size  (count array)
             index (mod (floor which) size)
             lower (select index array)
             higher (select (mod (+ index 1) size) array)]
         (switch (- which index) higher lower)))
  (:kr (let [index (floor which)
             size  (count array)
             lower (select:kr index array)
             higher (select:kr (mod (+ index 1) size) array)]
         (switch (- which index) higher lower))))

;; 2016.04.24
(defsound test option-d
  (m-map (fn [arr] (let [arr (map #(* (/ 1 %2) (saw (* %2 (midicps %))))
                                  arr (iterate #(+ % 0.99) 1))
                         speed (env-gen (envelope [1/5 1/5 2 80 1] [5 5 1 1]))
                         snd (select-c (rg-lin (lf-saw speed -1) 0 (count arr)) arr)]
                     snd))
         behind-the-mask))

;; 2016.04.24
(defsound test option-d
  (m-map (fn [arr] (let [arr (map #(pulse (* (mod %2 2) (midicps %))
                                          (rg-lin (lf-tri 3.2 (rand)) 0.1 0.5))
                                  arr (iterate #(+ % 0.99) 1))
                         speed (env-gen (envelope [1/5 1/5 2 80 1] [5 5 1 1]))
                         snd (select-c (rg-lin (lf-saw speed -1) 0 (count arr)) arr)]
                     snd))
         behind-the-mask))

;; 2016.04.25
(defsound test option-d
  (m-map #(let [freq (midicps (dt 0.7 %))
                snd (* (lf-pulse 8 %2 0.1)
                       (bpf (pink-noise) (rg-lin (lf-noise0 8) 200 1000) 0.5))]
            (ringz snd freq 0.4))
         behind-the-mask
         (n-range 7/8 1 3)))

;; 2016.04.25
(defsound test option-d
  (m-map #(let [freq (midicps (dt 1 %))]
            (switch (lf-pulse (dt:kr 1 [18 8 4]) 0
                              (a2k (rg-lin (sin-osc 0.3) 0.5 0.9)))
                    (* (sin-osc (/ freq 4)) (sin-osc (* 2 freq)))
                    (* (sin-osc (/ freq 6)) (sin-osc (* 4 freq)))))
         behind-the-mask))

;; 2016.04.26
(defsound test option-d
  (let [gate (impulse 1/3)]
    (m-map #(let [freq (midicps (dq gate %))
                  gate2 (dust:kr (env-gen (envelope [0 8 1] [0 1] EXP) gate))
                  fenv (env-gen (envelope [0 1 8 0] [0 2.95 0.05]) gate)]
              (+ (* 1/2 (env-gen (env-perc 9 0.5) gate)
                    (rlpf (saw (/ freq 2)) (* fenv freq) 0.25))
                 (* (env-gen (env-perc 0.01 0.3) gate2)
                    (sin-osc (* 8 freq)))))
           behind-the-mask)))

;; 2016.04.27
(defsound test option-d
  (let [gate (impulse 2/3)]
    (m-map #(let [freq (midicps (dq gate %))
                  fenv (env-gen (envelope [0 1.5 %2] [0 1.5]) gate)
                  env (env-gen (env-perc (- 0.5 %3) (+ 1.2 %3)) gate)]
              (* env (rlpf (lf-tri freq) (* fenv freq) 0.15)))
           behind-the-mask
           [18 2 12]
           [0.2 0 0.4])))

;; 2016.04.27
(defsound test option-d
  (let [gate (impulse (dt:kr 2 [1 1 1 1 2]))]
    (m-map #(let [freq (midicps (dq gate %))
                  fenv (env-gen (envelope [0 %2 %3] [0 1]) gate)
                  am-env (env-gen (envelope [0 24 9] [0 1]) gate)
                  env (env-gen (env-perc 0.9 0.1) gate)]
              (rlpf (* env (sin-osc (/ freq am-env)) (saw freq)) (* freq fenv) 0.2))
           behind-the-mask
           [1 2 18]
           [12 5 0.1])))

;; 2016.04.28
(defsound test option-d
  (let [lower (env-gen (envelope [0 4000 5000 1000 100] [0 1 1 0.2]) (impulse 1/3))
        snd (rlpf (white-noise) (rg-exp (white-noise) lower 5000) 0.5)]
    (m-map #(let [gate (impulse 1 %2)
                  freq (midicps (dq gate %))
                  env (env-gen (env-perc 0.05 0.5) gate)]
              (* env (ringz snd freq 0.05)))
           behind-the-mask
           (n-range 1/2 1 3))))

;; 2016.04.29
(defsound test option-d
  (pan2 (let [gate (impulse 1/3)]
          (m-map #(let [freq (midicps (dq gate %))
                        env (* (env-gen (envelope [0 1 1 0] [1 1.7 0.4]) gate)
                               (rg-lin (lf-tri 1.3 (rand)) 0.9 1))]
                    (* env (+ (pulse freq (rg-lin (lf-tri 1/3 (rand)) 0.1 0.9))
                              (* 1/4 (sin-osc (* freq 8))))))
                 behind-the-mask))))

;; 2016.04.29
(defsound test option
  (let [gate (impulse 1/3)]
    (g-verb (m-map #(let [freq (midicps (dq gate %))
                          env (* (env-gen (envelope [0 1 1 0] [1 1.7 0.4]) gate)
                                 (rg-lin (lf-tri 1.3 (rand)) 0.9 1))]
                      (* env (+ (pulse freq (rg-lin (lf-tri 1/3 (rand)) 0.1 0.9))
                                (* 1/4 (sin-osc (* freq 8))))))
                   behind-the-mask)
            0.1 0.8)))

;; 2016.04.30
(defsound test option
  (map (fn [detune]
         (m-map #(let [freq (midicps (+ detune (dt 1 %)))
                       gate (m-map lf-pulse %2)
                       env (env-gen (env-perc 0.03 (x-line:kr 0.1 1.2 10)) gate)]
                   (* env (sin-osc freq)))
                behind-the-mask
                [[1/2 1 6]
                 [1/2 3.3]
                 [1/2 4.8]]))
       [-0.1 0.01]))

;; 2016.04.30
(defsound test option
  (map (fn [detune]
         (m-map #(let [freq (midicps (+ detune (dt 1 %)))
                       gate (m-map impulse %2)
                       env (env-gen (env-perc 0.03 (x-line:kr 0.1 1.2 10)) gate)]
                   (* env (sin-osc freq)))
                behind-the-mask
                [[1/2 1.3]
                 [1/2 3.3]
                 [1/2 4.8]]))
       [-0.1 0.01]))

;; 2016.04.30
(defsound test option-d
  (m-map (fn [phase]
           (let [gate (impulse 2 phase)
                 octave (dq gate [1 1 2 1 2 2 3 3 2 3 2])
                 index (demand gate 0 (dbrown 0 3 1 INF))
                 arr (map #(sin-osc (* octave (midicps (dt:kr 2 %)))) behind-the-mask)
                 env (env-gen (env-perc 0.05 0.1) gate)]
             (* 4 env (select index arr))))
         (concat [0 1/4 1/2] (n-range 0 1 4))))


;; 2016.04.30 (surprise)
(defsound test option-d
  (m-map #(let [freq (midicps (dt 1 %))
                f-freq (dt 4 [5 10 80 400 800 1200])]
            (* 50 (bpf (bpf (white-noise) f-freq 0.1) f-freq 0.3)
               (switch (lf-tri 1/3)
                       (lf-tri (* freq 2))
                       (sin-osc freq))))
         behind-the-mask))

;; 2016.04.30
(defsound test option-d
  (m-map #(let [gate (impulse 1 %2)
                freq (midicps (dq gate %))]
            (* (step 1)                 ; need to avoid (rlpf (saw 0))
               (rlpf (saw (env-gen (envelope [0 freq (* 4 freq)] [0 0.3]) gate))
                     (env-gen (envelope [0 (* 4 freq) freq] [0 0.3]) gate) 0.1)
               (lf-pulse 6 0 (dt:kr 4 [1/2 1/4 3/4]))))
         behind-the-mask
         (n-range 0 1 3)))

;; 2016.05.01
(defsound test option-d
  (m-map #(let [gate (* %2 (impulse 16))
                freq (midicps (dt 2 %))]
            (* (env-gen (env-perc 0.01 0.3) gate) (sin-osc freq)))
         behind-the-mask
         [(* (lf-pulse 3) (lf-pulse 7) (lf-pulse 12))
          (* (lf-pulse 3) (lf-pulse 8) (lf-pulse 5))
          (* (lf-pulse 8) (lf-pulse 2))]))

;; 2016.05.01
(defsound test option-d
  (m-map #(let [gate (impulse 4)
                freq (midicps (dt:kr 1 %))]
            (* (env-gen (env-perc 0.00125 0.2) gate)
               (* 20
                  (cubed (saw (/ freq (dt 4 [2 4 8]))))
                  (rhpf (white-noise)
                        (env-gen (envelope [0 freq (* 4 freq)] [0 0.1] :step) gate)
                        0.00625))))
         behind-the-mask))

;; 2016.05.01
(defsound test option-d
  (m-map #(let [snd (* (sin-osc 7)
                       (white-noise))
                freq (* 4 (midicps (dt 1 %)))]
            (free-verb (* (rhpf snd freq 0.1)
                          (env-gen (env-perc 0.01 0.00001) (impulse 4)))
                       (x-line 0.1 1 5)
                       (x-line 0.1 10 12)))
         behind-the-mask))

;; 2016.05.02
(defsound test option-d
  (let [gate (impulse 1)
        [a b c] (map #(* %2 (midicps (dq gate %))) behind-the-mask (range 1 4))]
    (apply m-map #(* (sin-osc (switch (lf-pulse %3) %1 %2))
                     (env-gen (env-perc 0.05 0.8) gate))
           (transpose [[a b 3.5]
                       [b c 8]
                       [c a 2.5]]))))
;; 2016.05.03
(defsound test option-d
  (m-map (fn [phase]
           (let [gate (impulse 1 phase)
                 [a b c] (map #(* %2 (midicps (dq gate %))) behind-the-mask (range 1 4))]
             (apply m-map #(* (sin-osc (latch:ar (switch (lf-pulse %3) %1 %2)
                                                 (lf-pulse %3)))
                              (env-gen (env-perc 0.05 0.8) gate))
                    (transpose [[a b 3.5]
                                [b c 8]
                                [c a 2.5]]))))
         (n-range 1/3 1 4)))

;; 2016.05.03
(defsound test option-d
  (let [i (rg-lin (lf-saw 1 -1) 0 12)
        octave (+ 1 (floor (/ i 3)))
        arr (map #(midicps (dt 1 %)) behind-the-mask)
        freq (* octave (select (mod i 3) arr))]
    (lpf (sync-saw freq (* (rg-exp (lf-tri 0.7) 2 3) freq))
         (line 2000 50 10))))

;; 2016.05.03
(defsound test option-d
  (m-map #(let [gate (impulse (dt:kr 1 [4 4 8 1]))
                long (dt:kr 1 [0.2 0.2 0.12 0.5])
                freq (midicps (dq gate %))]
            (* (rlpf (white-noise) (* 4 freq) 0.025)
               (env-gen (env-perc 0.00001 long) gate)))
         behind-the-mask))

;; 2016.05.04 (strong)
(defsound test option-d
  (let [pat (concat [1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0]
                    [1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0]
                    [1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0]
                    [1 1 1 1 0 0 0 0 1 1 1 1 1 1 0 0]
                    [1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0])
        gate (pattern (impulse 16 [0 1/4]) pat)
        freq (env-gen (envelope [0 3000 700] [0 0.001] :step) gate)]
    (+ (* (rlpf (white-noise) freq 0.5) (env-gen (env-perc 0.0005 0.1) gate))
       (let [gate (impulse 1)]
         (* (m-map #(let [freq (* %2 2 (midicps (dq gate %)))]
                      (ringz (brown-noise) freq
                             (env-gen (envelope [0 0.01 0.8] [0 0.3] :exp)
                                      gate)))
                   behind-the-mask
                   (iterate inc 2))
            (env-gen (env-perc 0.03 0.5) gate)))) ))

;; 2016.05.30
(defmacro hoge [params-def params body]
  (let [[_ & forms] (synth-form 'a [params-def body])]
    `(let [synth# (synth "audition-synth" ~@forms)]
       (letfn [[f# [current# [hd# & params#]]
                (synth# hd#)
                (when-not (empty? params#)
                  (apply-by (+ 1000 current#) f# [(+ 1000 current#) params#]))]]
         (f# (now) ~params)))))
(hoge [freq 100]
      (range 300 800 100)
      (out 0  (* (env-gen (env-perc 0.05 0.5) :action FREE) (sin-osc freq))))


;; 2016.05.30
(defsound test option-d
  (mm-map (fn [freq]
            (let [freq (* freq
                          (env-gen (envelope [1 1 2 0] [0.5 0.3
                                                        (ranged-rand 0.5 3)])
                                   (impulse 1/3)))]
              (* (sin-osc (sin-r 3.2 freq [(* 1.01 freq)
                                           (* 0.99 freq)]))
                 (sin-r 1.2 [0.9 0.8]) (sin-r 1/8 [0.3 0.1]))))
          (n-range 800 1000 5)))

;; 2016.05.30
(defsound test option
  (let [source (white-noise)
        freq (sin-r (sin-r 1/3 0.01 8) 1000 [1600.0 1800.0])]
    (* (sin-r 3.3 0.75)
       (moog-ff source freq 1))))

;; 2016.06.07
(defsound test option-d
  (let [gate (impulse 2)
        vol (dq gate [[1 1/4 1/4 1/4]
                      [1/2 1/4 1/4 1/4]
                      [1/2 1/16 1/4 1/4]
                      [1/2 1/4 1/4 1/4]])
        len (dq gate [[0.001 0.004 0.004 0.001]
                      [0.01 0.004 0.004 0.001]
                      [0.01 0.02 0.004 0.001]
                      [0.01 0.004 0.004 0.001]])
        len2 (dq gate [1e-4 1e-6 1e-6 1e-3])
        attack (env-gen (envelope [0 1 1 0] [1e-7 len len2]) gate)]
    (m-map #(* vol %2 (ringz (* (white-noise) attack) % %3))
           [100 300 380 600 800.0]
           [1 1 2/3 1/3 1/8]
           [0.184765 0.284765 0.1423825 0.1423825])))

;; 2016.06.08
(defsound test option
  (let [snd (switch [(lf-pulse (line:kr 1 10 10))
                     (clip:ar (lf-tri 1/3) -0.2 0.2)]
                    (brown-noise)
                    (white-noise))]
    (rlpf snd 1000 (sin-r (sin-r 1/3 1 20) 0.01 0.5))))

;; 2016.06.09 (like)
(defsound test option-d
  (let [freq (latch:ar (sin-r 30 100 3000)
                       (impulse (sin-r:kr 1/3 4 32)))]
    (* (sin-osc freq) (env-gen (env-perc 0.05 0.3) (impulse 4)))))

;; 2016.06.10
(defsound test option
  (let [freq (sin-r (sin-r 4 1 12) 200 800)
        freq [(* 1.01 freq) (* 0.99 freq)]
        env (lin-lin (m-map lf-pulse [1 1/2 1/8]) 0 1 0.1 1)]
    (* (+ (sin-osc (/ freq 3))
          (pulse freq
                 (rg-lin (lf-saw 5.5) 0.5 1))) env)))


;; 2016.06.20
(defsound test option
  (let [freq (lag-ud (dt:kr 1/2
                            [440 540 440 540 440 540
                             660.0 810.0]) 0.5 0.3)
        freq [(* 1.001 freq)
              (* 1.00 freq)]
        vol (rg-exp (sin-osc 1/3) 0.125 1)]
    (* 1/8
       (+ (sin-osc freq)
          (* 1/4 (- 1 vol) (pulse (* 4 freq) (rg-lin (sin-osc 2/3) 0.1 0.5)))
          (* vol (saw (* 2 freq)))))))

;; 2016.06.20
(defsound test option
  (let [gate (delay-n:kr (impulse:kr 8)
                         0.02 (dt:kr [1/4 3/4] [0 0.015]))]
    (* (white-noise) (env-gen (env-perc 0.01 0.05) gate))))

;; 2016.06.25
(defsound test option-d
  (m-map #(let [freq (env-gen
                      (envelope
                       [(+ 1000 (* % 1000)) 1000 (- 1000 (* % 200))]
                       [3 1]) :action FREE)]
            (* (sin-osc (line 10 200 4))
               (pulse freq (sin-r 3.2 0.1 0.5))))
         (range 1 5)))



;; 2016.06.29 (rythm)
(defsound test option-d
  (let [gate (impulse (dt:kr 4 [8 8 8 9 9 11 16]))
        vol (dq gate [[1 1/4 1/4 1/4]
                      [1 1/4 1/4 1/4]
                      [1/16 1/4 1/4 1/4]
                      [1 1/4 1/4 1/4]])
        length (dq gate [[0.3]
                         (repeat 7 0.1)
                         [0.03]
                         (repeat 7 0.1)])]
    (* vol (white-noise) (env-gen (env-perc 0.001 length) gate))))


;; 2016.07.01 (rythm)
(defsound test option-d
  (let [gate (impulse 8)
        sounds (map (fn [freqs]
                      (m-map #(rlpf (brown-noise) % 0.1) freqs))
                    [[1000 1800 3000]
                     [1300 1700 2500]
                     [600 800 3500]
                     [2700 3100 3600]])
        idx (dq gate [[1 2 1 4 1 2 1 4]
                      [1 2 1 4 3 3 1 4]
                      [1 2 1 4 3 3 1 4]
                      [2 2 2 2 4 4 4 4]
                      [1 2 1 4 1 2 1 4]])
        vol (dq gate [ (repeat 6 [1 1/4 1/4 1/4])
                      [[1 1 1/2 1/2] [1 1 1/2 0]]
                      (repeat 2 [1 1/4 1/4 1/4])])]
    (* (select idx sounds) vol
       (env-gen (env-perc 0.001 0.05) gate))))

;; 2016.07.21
(defsound test option-d
  (apply m-map #(* (saw (sin-r %4 10 %3))
                   (env-gen (env-perc (x-line:kr 0.0001 0.3 10) %2)
                            (impulse 4 %)))
         (transpose [[0 0.5 800 300]
                     [0.9 0.3 1000 30]
                     [0.7 0.1 1000 30]
                     [0.5 0.4 1200 200]
                     [0.3 0.03 2500 80]
                     [0.1 0.1 600 80]
                     [0.15 0.15 600 80]])))

;; 2016.07.24
(defsound test option-d
  (let [fenv (env-gen (envelope [0 3 1] [0 0.005]))
        gate (impulse 10 (sin-r:kr 1 0 0.5))
        freqs (map #(dt 1 %) (transpose [[1000 1100 1700]
                                         [800 1600 1700]
                                         [580 1800 2550]]))
        env (env-gen (env-perc 0.01 0.3) gate)
        rq-env (env-gen (envelope [0 0.01 0.01] [0 0.01] EXP) gate)]
    (* env
       (mix (rlpf (cubed (white-noise))
                  (* fenv freqs)
                  rq-env)))))
;; 2016.08.22
(defsound test option
  (let [ratio (x-line 1.1 1.4 18)
        base-freq 140
        arr (take 10 (iterate #(* ratio %) base-freq))]
    (splay (map #(let [gate (impulse 1 %2)]
                   (* 8 (lf-tri (* % (+ 1 %2)
                                   (+ 1 %2)))
                      (env-gen (env-perc 0.05 0.5) gate)))
                (concat arr (reverse arr))
                (range 0 1 1/20)))))

;; 2016.08.23
(defsound test option-d
  (let [vol (lf-noise0 16)
        snd (* vol (rlpf (cubed (brown-noise))
                         (lag-ud (rg-lin (lf-noise0 (line 1 8 8)) 500 2000))))]
    (+ (* (- 1 snd) vol (clip2 (sin-osc (line 440 420 8)) 0.98)) snd)))

;; 2016.08.25
(defsound test option-d
  (let [base-freq (dt 1 [500 520 550 800 1200 2400 4300 300])
        max-freq 1800
        freq (switch (switch (lf-pulse 1) (brown-noise) (pink-noise))
                     (rg-lin (lf-pulse 16) base-freq max-freq)
                     (rg-lin (lf-pulse 12) (* 1.05 base-freq) max-freq))]
    (tanh (ringz (cubed (pink-noise)) freq 0.4))))

;; 2016.08.26
(defsound test option
  (splay (map #(let [gate (* (step %) (impulse (dt:kr 1 [1 2 4])))
                     freq (dq gate (map (fn [x] (+ x %)) [400 480 580 350]))
                     ratio (dq gate [3/2 3/2 3/2 3/2 8/3 9/4 11/4])
                     env (env-gen (envelope [0 1 1 0] [0.01 0.2 0.05]) gate)]
                 (* env (lf-tri (* [1 ratio] freq))))
              (concat [0 2] (n-range 6 12 6)))))

;; 2016.08.27
(defsound test option-d
  (rlpf (brown-noise)
        (lin-lin (tanh (* 20 (lf-noise2 4))) 0 1 100 1200)
        0.1))

;; 2016.08.27 (low)
(defsound test option-d
  (let [freq (sin-r 0.4 80 150)]
    (m-map #(* (/ 1 %) (lf-tri  (* dr % freq))) (n-range 1 30 50))))

;; 2016.08.28
(defsound test option-d
  (let [snd (saw (rg-exp (lf-saw (lin-exp (m-map lf-pulse [1 32 10 7]) 0 1 0.1 100))
                         100 20000.0))]
    (ringz snd (sin-r 100 1 30) (sin-r 1/2 0 1))))

;; 2016.08.29
(defsound test option-d
  (let [freq (rg-exp (lf-tri:kr 1/5 2) 1/2 16.0)
        gate (impulse freq)]
    (* (pink-noise) (env-gen (env-perc 0.01 (min 0.2 (/ 1 freq))) gate))))

;; 2016.09.01
(defsound test option-d
  (let [freq (rg-exp (lf-tri:kr 1/5 2) 1/2 16.0)
        inv-freq (/ 1 freq)
        ratio (lin-exp freq 1/2 16.0 1 8)
        gate (impulse freq)]
    (* (saw (* ratio 440)) (env-gen (env-perc 0.01 (min 0.3 (/ 1 freq))) gate))))

;; 2016.09.02
(defsound test option
  (let [gate (m-map #(let [freq (lin-exp % -1 1 1/2 16.0)] (impulse freq))
                    [(lf-tri:kr 1/6 2)
                     (lf-tri:kr 1/3 2)])]
    (* (white-noise) (env-gen (env-perc 0.01 0.1) gate))))
;; 2016.09.03
(defsound test option-d
  (m-map #(let [freq (rg-exp (lf-tri:kr %2 2) 1/2 16.0)
                gate (impulse freq)]
            (* %1 (env-gen (env-perc 0.0 (/ 1 freq)) gate)))
         [(cubed (white-noise)) (brown-noise) (lpf (white-noise) 150)]
         [1/2 1 1/5]))
;; 2016.09.4
(defsound test option
  (let [gate (impulse 2)
        gate2 (impulse (env-gen (envelope [2 2 2.1 2] [5 10 5])))
        freq [(dq gate [200.0 300.0])
              (dq gate2 [50.0 350.0])]]
    (map #(* (sin-osc %)
             (env-gen (env-perc 0.001 0.5 :curve 3) %2))
         freq
         [gate gate2])))

;; 2016.09.05
(defsound test option
  (let [gate (impulse 2)]
    (splay (map #(let [delay (switch (lf-pulse 1) 0 %2)
                       gate2 (delay-n:kr gate 0.1 delay)
                       freq (dq gate %1)]
                   (* (sin-osc freq)
                      (env-gen (env-perc 0.05 0.5) gate2)))
                [[440 500] [442 530] [448 580]]
                [(rg-lin (lf-saw:kr 1/5 -1) 0 0.1)
                 (rg-lin (lf-saw:kr 1/5 -1.1) 0 0.1)
                 (rg-lin (lf-saw:kr 1/5 -1.2) 0 0.1)]))))

;; 2016.09.08 (rythm)
(defsound test option
  (splay (map #(let [gate (impulse %)
                     fenv (env-gen (envelope [0 2 1] [0 0.01]) gate)
                     freq (* fenv %2)]
                 (* 3 (rlpf (white-noise) freq 0.15) (env-gen (env-perc 0.01 0.1) gate)))
              [7/3 4/3 5]
              [200 650 700])))

;; 2016.09.09 (rythm)
(defsound test option
  (splay (map #(let [gate (impulse 4 %)
                     vol (latch:ar (lin-exp (* (sin-osc 1/3) (lf-saw 2)) -1 1 0.1 1) gate)]
                 (* 8 (rlpf (brown-noise) %2 0.1) (env-gen (env-perc 0.01 0.2) gate)
                    vol))
              (range 0 1 1/4)
              [400 200 1230 900])))

;; 2016.09.10 
(defsound test option
  (splay (map #(let [freq %
                     arr (* 1/3 (range %2 (+ %2 3)))
                     snd (switch (lf-pulse 1/7)
                                 (rlpf (white-noise) freq 0.1)
                                 (saw freq))
                     gate (m-map impulse arr)
                     vol (latch:ar (lin-exp (m-map lf-pulse arr) 0 1 0.01 1) gate)]
                 (* 4 vol snd (env-gen (env-perc 0.03 0.15) gate)))
              [400 480 880 2200]
              (range 2 6))))

;; 2016.09.16
(defsound test option-d
  (* (sin-osc (sin-r 3.3 100 4000)) (impulse (line:kr 20 100))))
;; 2016.09.16
(defsound test option-d
  (tanh (moog-ff (white-noise) 1300 3.95)))
;; 2016.09.16
(defsound test option-d
  (let [ratio (latch:ar (line 0 1 6) (impulse 1/2))]
    (* 40 (m-map #(* (/ 1 %) (bpf (white-noise) (* 800 % (switch ratio %2 1)) 0.005))
                 (map #(+ (* 2 %) 1) (range 0 4))
                 [1 1.05 1.2 1.4]))))

;; 2016.09.17 (rythm)
(defsound test option
  (splay (let [trig (impulse 8)
               gate [(pattern trig [0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0])
                     (pattern trig [1 0 0 0 1 0 1 0 1 1 1 1 0 0 1 0])
                     (pattern trig [0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0])]
               freq [800 300 2200]
               fenv (env-gen (envelope [0 8 1 2] [0 0.01 0.02]) gate)]
           (* 4 (rlpf (brown-noise) (* freq fenv) 1) (env-gen (env-perc 0.01 0.5) gate)))))

;; 2016.09.17 (rythm)
(defsound test option
  (let [trig (impulse (rg-lin (lf-pulse 1/4 0 7/8) 16 8))]
    (splay (let [gate [(pattern trig [1 0 1 0])
                       (pattern trig [0 1 0 0 0 0 0 1])
                       (pattern trig [1 0 1 0 1 0 1 0])]
                 freq [200 1800 4200]
                 fenv (env-gen (envelope [0 5 1] [0 0.01]) gate)]
             (* (bpf (white-noise) (* freq fenv)) (env-gen (env-perc 0.001 0.05) gate))))))

;; 2016.09.18 (rythm)
(defsound test option 
  (let [trig (impulse 8)
        gate [(pattern trig [[1 1 1 1]
                             [1 1 1 1]
                             [0 1 1 1]
                             [1 1 1 1]])
              (pattern trig [[0 0 0 0]
                             [0 0 1 0]
                             [1 0 0 0]
                             [1 1 1 1]])]
        freq [9800
              (sin-r 1/5 16000 18000)]
        vol [2 1.5]]
    (* vol (tanh (moog-ff (white-noise) freq 3.75))
       (env-gen (env-perc 0.01 0.02) gate))))

;; 2016.09.18
(defsound test option-d
  (let [thr 0.2
        cliped (clip (lf-tri:kr 1/4 -1) 0 thr)
        base (lin-lin cliped 0 thr 800 4300)
        extra (rg-exp (sin-osc (lin-lin cliped 0 thr 17 0.1)) 1200 1)
        freq (+ extra base)]
    (sin-osc  (* (sin-r freq 0.5 4) freq))))

;; 2016.09.19 
(defsound test option-d
  (mix (let [trig (impulse 6)
             gate [(pattern trig [[1 0 0 0 1 0 0 0]
                                  [1 0 0 0 1 0 0 0]
                                  [1 0 0 0 1 0 0 0]
                                  [1 0 1 0 1 1 0 0]])
                   (pattern trig [[0 0 0 0 0 0 0 0]
                                  [0 1 0 0 0 1 0 0]
                                  [1 1 0 0 1 1 0 0]
                                  [1 0 1 0 0 0 1 1]])
                   (pattern trig [[0 0 0 0 0 0 0 0]
                                  [0 0 0 0 0 0 0 1]
                                  [0 0 0 0 0 0 1 0]
                                  [1 0 0 0 1 0 1 0]])]
             freq [1300 800 3200]]
         (* (sin-osc freq) (env-gen (env-perc 0.05 0) gate)))))

;; 2016.09.20 (rythm)
(defsound test option-d
  (let [trig (impulse 4)]
    (m-map #(let [gate %3
                  freq (* %2 (env-gen (envelope [0 8 1] [0 0.02]) gate))]
              (* (rlpf (white-noise) freq 0.1) (env-gen (env-perc 0.05 0.13) gate)))
           (n-range 5/6 1 3)
           [1500 2000 8000]
           [(pattern trig [1 0 0 0 1 0 1 0])
            (pattern trig [1 0 1 0 0 0 0 1])
            (pattern trig [0 0 0 1 0 1 0 0])])))
;; 2016.09.21 (India: ジャプ・タール)
(defsound test option-d
  (let [gate (impulse 4)
        dhi 1200
        na 800
        freq (dq gate [dhi na dhi dhi na 0 na dhi dhi na])
        vol (dq gate [1 1/8 1/2 1/8 1/8 0 1/8 1/2 1/8 1/8])]
    (* vol 8
       (bpf (white-noise) freq 0.1)
       (env-gen (env-perc 0.02 0.2) gate))))

;; 2016.09.22
(defsound test option
  (pan2 (let [gate (impulse 1/2)
              base-freq (dq gate [1620.0 1400 800])
              fenv (env-gen (envelope [0 1.2 1 1.015] [0 0.01 0.5] gate))
              freq (* fenv (sin-r 1.2
                                  base-freq
                                  (* 1.013 base-freq)))
              vib (sin-r 1.3 (line 1 0.85 2) 1)
              env (env-gen (envelope [0 1 1 0] [0.3 0.6 1]) gate)
              snd (+ (* 1/64 (lf-tri (/ freq 4 (sin-r 20.0 0.94 1))))
                     (lpf (saw freq)
                          (* freq (env-gen (envelope [1 5.0 1] [0.2 2]) gate))))]
          (* env vib snd))))

;; 2016.09.22 (rythm)
(defsound test option
  (pan2 (let [gate (impulse (dt:kr 1/2 [[2 2 2 4]
                                        [2 2 8 2]
                                        [2 2 4 2]
                                        [2 2 2 2]
                                        [4 4 8 8]]))
              which (lf-pulse 1)
              snd (switch which
                          (brown-noise)
                          (hpf (white-noise) 2300))
              env (switch which
                          (env-perc 0.01 0.07)
                          (env-perc 0.01 0.1))]
          (* snd (env-gen env gate)))))

;; 2016.09.23
(defsound test option-d
  (m-map #(* 3 (sin-r 0.3 0.5 1)
             (sin-r 0.8 0.2 1)
             (lag-ud (cubed (rg-lin (lf-saw -30 -1) 0 1)))
             (lf-tri %))
         (repeatedly 8 #(ranged-rand 100 300))))

;; 2016.09.23 (fly)
(defsound test option-d
  (mix (repeatedly
        10
        #(let [gate (dust:kr (line:kr 100 10 10))]
           (tanh (* (rlpf (white-noise) (lin-exp (brown-noise) 0 1 7000 28000) 0.01)
                    (env-gen (env-perc 0.01 0.1) gate)))))))

;; 2016.09.23
(defsound test option-d
  (* (clip:ar (lf-noise2 2) 0.1 0.6)
     (sin-osc (* (lin-exp (clip:ar (lf-noise2 6) 0 0.6) 0 0.6 800 5000)
                 (sin-osc (lin-exp (clip:ar (lf-noise2 3) 0.8 1) 0.8 1 10 1000))))))
;; 2016.09.23
(defsound test option-d
  (tanh (* (clip:ar (lf-noise2 2) 0.1 0.6)
           (saw (lin-exp (clip:ar (lf-noise2 6) 0 0.6) 0 0.6 500 2000))
           (sin-osc (lin-exp (clip:ar (lf-noise0 3) 0.8 1) 0.8 1 10 800)))))

;; 2016.09.24
(defsound test option-d
  (let [gate (impulse 1)
        freq (+ (x-line 10 2000 20) [(sin-r 1.3 1 5)
                                     (sin-r 2.3 -1 -5)])
        snd (* (cubed (lf-pulse 8 0 (line:kr 1 1/16 20)))
               (hpf (white-noise) 8000))]
    (comb-l (tanh (+ snd (comb-l snd 1 (/ 1 freq) 0.5)))
            0.3 0.3 1)))

;; 2016.09.26 (instant)
(defsound test option-d
  (let [buf (local-buf 1)
        x (mod (+ (ranged-rand 0.01 1) (dbufrd buf)) 1.0)
        snd (dbufwr x buf)]
    (tanh (free-verb (duty 0.008 0 snd) 1 1))))

;; 2016.09.26(low)
(defsound test option-d
  (let [gate (impulse 2)
        fenv (env-gen (envelope [0 5 1] [0 0.013]) gate)
        snd (sin-osc (* fenv (sin-r 50 20 180)))]
    (+ (* snd (env-gen (env-perc 0.01 0.3) gate))
       (* 1/8 (lpf (white-noise) (env-gen (envelope [0 6000 500] [0 0.02]) gate))
          (env-gen (env-perc 0.02 0.5) gate)))))

;; 2016.09.28
(defsound test option-d
  (m-map #(let [gate (impulse 2 %)
                freq (dq gate (range %2 (+ 1000 %2) 100))
                f-env (env-gen (envelope [0 1 1 1.5] [0 0.05 0.001]) gate)
                env (env-gen (env-perc 0.05 0.5) gate)]
            (* (sin-osc (* freq f-env)) env))
         (n-range 0 1 10)
         [300 850 1200]))

;;2016.09.30
(defsound test option-d
  (m-map #(let [gate (impulse 4 %)
                freq (dq gate (range %2 (+ 1000 %2) 100))
                f-env (env-gen (envelope [0 1 1 1.5] [0 0.05 0.001]) gate)
                env (env-gen (env-perc 0.05 0.2) gate)]
            (* (sin-osc (* freq f-env)) env))
         (n-range 0 1 10)
         (take 10 (iterate #(* 1.5 %) 100))))


;; 2016.10.02(horse)
(defsound test option-d
  (let [trig (impulse (dt:kr [4 1] [12 24]))
        gate (switch (lf-pulse 1/5 0 4/5)
                     (pattern trig [1 1 0 0])
                     trig)
        freq (dq gate [1000 800])
        f-env (env-gen (envelope [0 2 1] [1e-8 4e-3]) gate)]
    (* 8 (bpf (brown-noise) (* f-env freq) 0.2)
       (env-gen (env-perc 0.01 0.1) gate))))

;; 2016.10.05 (rythm)
(defsound test option-d
  (m-map #(let [gate %1
                freq %2
                f-env (env-gen (envelope [0 8 1] [1e-4 0.012]) gate)
                freq (* f-env freq)
                rq (env-gen (envelope [0 1 0.01] [1e-5 0.2]) gate)]
            (* (bpf (white-noise) freq rq) (env-gen (env-perc 1e-5 0.5) gate)))
         [(pattern (impulse 4) [[1 0 0 0]
                                [1 0 0 0]
                                [1 1 0 0]])
          (pattern (impulse 8) [[0 0 1 0] [0 0 1 0]
                                [0 1 0 0] [0 1 0 0]
                                [0 0 1 1] [0 0 1 1]])
          (impulse 2)]
         [800 2000 450]))

;; 2016.10.07
(defsound test option
  (map #(let [dur 1/8
              gate (impulse 8 %)
              trig (pattern gate [1 0 0 0 0 0 0 0])
              freq %2
              f-env (env-gen (envelope [1 1 1 1.1 1.1 1 1 0.65]
                                       [dur dur dur dur dur dur dur]) trig)]
          (* (rlpf (white-noise) (* freq f-env) 0.1)
             (env-gen (env-perc 0.001 0.2) gate)))
       [0 0]
       [400 1200]))

;; 2016.10.08(low)
(defsound test option-d
  (mix (let [gate (pattern (impulse 8) [1 0 0 0 0 1 0 1 0 0 0])
             env [(env-gen (env-perc 0.01 (dq gate [0.16 0.1 0.2])) gate)
                  (env-gen (env-perc 0.05 (dq gate [0.5 0.1 0.3])) gate)
                  (env-gen (env-perc 0.05 (dq gate [1 0.2 0.4])) gate)]
             freq [800 350 110]
             vol [0.5 0.35 1]]
         (* (sin-osc (* freq (sin-osc [80 38 18])))
            vol env))))

;; 2016.10.10
(defsound test option-d
  (let [gate (impulse 1/2)
        freq (dq gate [40 50 25 30])]
    (pulse (rg-lin (lf-pulse 6) freq (* 1.4 freq)))))

;; 2016.10.11
(defsound test option
  (detune 30 (fn [detune]
               (m-map #(let [gate (impulse 1 (/ 1 %))
                             f-freq %2
                             f-freq (* (env-gen (envelope [0 4 1] [0 0.1]) gate)
                                       f-freq)
                             freq (+ 400 detune)]
                         (* (rlpf (pulse %2 (sin-r 3.5 0.8 1/2)) f-freq)
                            (env-gen (env-perc (x-line:kr 0.01 0.5 10) 0.5) gate)))
                      (range 2 7)
                      (n-range 1020 6000 7)))))

;; 2016.10.12
(defsound test option
  (detune 30 (fn [detune]
               (let [gate (impulse 1)
                     f-env (env-gen (envelope [0 8 1] [0 2e-2]) gate)
                     freq (+ 600 detune)
                     freq (* freq f-env)]
                 (* (rlpf (brown-noise) freq 0.1)
                    (env-gen (env-perc 0.05 0.5) gate))))))

;; 2016.10.14
(defsound test option
  (detune 10 (fn [detune]
               (let [gate (impulse (dt:kr 2 [4 8 4 4 1]))
                     freq (dt:kr 1 [440 440 440 400 700])]
                 (* (sin-osc (+ freq detune))
                    (env-gen (env-perc 0.08 0.4) gate))))))

;;2016.10.16
(defsound test option
  (detune 1 (fn [detune] (m-map #(let [freq (dt 1 [400 380 360 720])
                                       freq (+ freq detune)
                                       freq (* freq %)]
                                   (rlpf (saw freq) (sin-r 1/3 1000 9000) 0.2))
                                (map #(/ % (+ % 1)) (range 1 3))))))

;; 2016.10.18
(defsound test option
  (detune 100  (fn [detune]
                 (let [snd (* (lf-noise0 8) (white-noise))
                       freq (dt 1/2 [1000 1000 1000 1400 4000])
                       rq (sin-r (rg-lin (lf-saw 1) 0.1 3.2) 0.01 0.1)]
                   (rlpf snd (+ detune freq) rq)))))
;; 2016.10.24
(defsound test option-d
  (mix (let [gate (map #(* (impulse %) (step %2))
                       [3 1 2 3.5]
                       [0 3 5 7])]
         (* (sin-osc (sin-r 440 100 1000))
            (env-gen (env-perc 0.05 0.5) gate)))))

;; 2016.10.26 (perc)
(defsound test option
  (let [gate (pattern (impulse 4) [1 0 0 0 0 0 0 0 1 1 0 0 ])]
    (detune  50 (fn [detune]
                  (* (env-gen (env-perc 0.01 0.1) gate)
                     (pulse (+ detune 1200))
                     (brown-noise))))) )

;; 2016.11.02
(defsound test option
  (splay (map #(let [gate (impulse 1 %)]
                 (* 30
                    (sin-osc %2)
                    (env-gen (env-perc 0.05 0.5) gate)
                    (lf-pulse %3 0 (env-gen (envelope [0.95 0.5] [0.3]) gate))))
              (n-range 0 0.5 10)
              (shuffle (take 10 (iterate #(* 1.1 %) 300)))
              (cycle [64 10 18 4]))))

;; 2016.11.08
(defsound test option-d
  (let [gate (impulse (dt:kr 1 [1 1 1 4]))
        freq (dq gate [[1000 2000.0]
                       [1000 2000.0]
                       [1000 2000.0]
                       [3800 800.0 3800 800.0 3800 800.0 3800 800.0]])]
    (* (rlpf (white-noise) freq) (env-gen (env-perc 0.05 0.5) gate)
       (lf-pulse 24))))

;; 2016.11.13
(let [arr (repeatedly 8 #(ranged-rand 100 8000))]
  (defsound test option
    (splay (repeatedly 8 #(let [gate (dust:kr 5)
                                freq (demand gate 0 (drand arr INF))]
                            (* 5 (sin-osc freq)
                               (env-gen (env-perc 1e-6 3e-2) gate)))))))

;; 2016.11.14
(let [arr (repeatedly 16 #(ranged-rand 50 600))]
  (defsound test option
    (splay (repeatedly 16 #(let [gate (dust:kr 3)
                                 freq (demand gate 0 (drand arr INF))]
                             (* (rg-lin (lf-pulse (line:kr 1/2 10)) 1 3) 20
                                (sin-osc freq)
                                (env-gen (env-perc 1e-3 1e-1) gate)))))))

;; 2016.11.17
(defsound test option-d
  (let [gate (pattern (impulse 8) [[1 0 1 0]
                                   [1 0 1 0]
                                   [1 0 1 0]
                                   [1 0 0 0]
                                   [1 0 1 0]
                                   [0 0 0 1]])
        attack (dq gate [0.05 0.01])
        release (dq gate [0.5 0.1 0.5])
        freq (env-gen (envelope [440 440 480 10000 440 440 220] [5 3 1.5 0.5 3 3]))]
    (+ (* (sin-osc freq) (env-gen (env-perc attack release) gate))
       (* 1/8 (+ (sin-osc 220) (sin-osc (* 220 16 (sin-osc 440))))
          (sin-r 3.2 0.8 1)
          (env-gen (env-perc 0 4) (impulse 1/4 1/2))))))

;; 2016.11.17
(defsound test option-d
  (let [gate (impulse (dt:kr 1 [1 1 1 1 4 1/4 4 8]))
        freq (dq gate (flatten [(take 4 (cycle [440 880]))
                                (n-range 330 880 8)]))
        arr (transpose [[0.08 0.25 0.2] [0.08 0.25 0.2] [0.08 0.25 0.2] [0.08 0.25 0.2]
                        [0.01 0 0.1] [0.1 0.4 0.4] [0.01 0 0.1] [0.01 0 0.1]])
        [attack dur release] (map #(dt:kr 1 %) arr)]
    (+ (* (sin-osc freq) (env-gen (envelope [0 1 1 0] [attack dur release]) gate))
       (* (pink-noise) (env-gen (env-perc 0.05 0.8) (impulse 1/4 1/4)))
       (* (white-noise) (env-gen (env-perc 0 0.05) (impulse (dt:kr 4 [8 1])))
          1/4 (lf-pulse 1/4 0 3/4)))))

;; 2016.11.18
(defsound test option-d
  (let [base (dt:kr 1 [1 1 1 2 1 1 4 1/2])
        freq (dt:kr 1 [440 660 880 1800])
        i-base (/ 1 base)]
    (+ (let [gate (impulse base)]
         (* 3/2 (lf-pulse 8) (rlpf (brown-noise) freq 0.1) (env-gen (env-perc 0.05 (* 0.5 i-base)) gate)))
       (let [gate (impulse base 1/2)
             half (* 0.25 i-base)
             f-env (env-gen (envelope [0 12 3 8] [0 half half] EXP) gate)]
         (* 2/3 (rlpf (white-noise) (* f-env freq 1/2) 0.3)
            (env-gen (env-perc (* 0.5 i-base) 0.05) gate))))))

;; 2016.11.19
(defsound test option
  (let [freqs (repeatedly 16 #(exp-rand 100 8000))
        freq [(dt 1 freqs)
              (dt 1 (rotate 1 freqs))]
        vol (dt 1/2 [1 1/2 1 1/2 1/8 1/16])]
    (+ (splay (sin-osc freqs))
       (* vol (sin-osc freq) (env-gen (env-perc 0.05 0.5) (impulse 2 [0 1/2]))))))
;; 2016.11.21
(defsound test option-d
  (let [base (m-map lf-pulse [1/4 1/3 1/8])
        gate (m-map impulse (n-range 1 2 8))]
    (tanh (+ (* (sin-osc (lin-lin base 0 1 600 1800)) (env-gen (env-perc 0.008 0.13) gate))
             (let [gate (impulse (dt:kr 4 [1 1/2 2/3 3/2]))]
               (* (pulse (lin-lin base 0 1 6800 8800)) (env-gen (env-perc 0.05 0.5) gate)))
             (let [freq (lag-ud (lin-exp base 0 1 600 8000) 0.5 0.5)]
               (* (moog-ff (saw 100) freq 3.99) 1/2
                  (env-gen (env-perc 0.15 0.6) (pattern gate [1 0 0 0]))))))))

;; 2016.11.22
(defsound test option-d
  (let [gate (* (lf-pulse 1 0 7/8) (impulse 32))
        freq (dq gate [1000 600])
        gate2 (impulse 1)
        freq2 (env-gen (envelope [0 12000 3000 100] [0 6/8 1/8]) gate2)]
    (tanh (+ (* (rlpf (brown-noise) freq) (env-gen (env-perc 0.04 0.25) gate))
             (* 1/8 (rlpf (pink-noise) freq2 0.3) (env-gen (env-perc 0.99 0.01) gate2))
             (* (sin-r 1/6 1/4 1)
                (rlpf (saw 80) (rg-exp (sin-osc (dt 2 [1/8 1/4 1/4 2])) 100 1000) 0.01))))))

;; 2016.11.23
(defsound test option-d
  (let [gate (impulse (dt:kr 1 [1 1 2 4 8]))]
    (+ (apply m-map #(let [freq (lin-lin (mix [(lf-tri 1/4) (lf-pulse 2) (lf-saw 1/8)]) 0 1 % %2)]
                       (* (sin-osc freq) (env-gen (env-perc 0.05 0.5) gate)))
              (transpose [ [200 1200] [1800 8000] [2400 3200] [50 600]]))
       (* (white-noise) (saw (dq gate [8300 1200])) (env-gen (env-perc 0.01 0.1) gate)))))

;; 2016.11.24
(defsound test option-d
  (let [gate (impulse (dt:kr 1 [1 1 2 4 8]))]
    (apply m-map #(let [freq (lin-lin (mix [(lf-tri 3/2) (lf-pulse 1/4) (lf-saw 1)]) 0 1 % %2)]
                    (tanh (+ (* 1/4 (rlpf (white-noise) freq 0.15) (env-gen (env-perc 0.05 0.5) gate))
                             (tanh (* (ringz (* (brown-noise) gate) (latch:ar freq gate) 0.18) )))))
           (transpose [ [200 1200] [1800 8000] [2400 3200] [50 600]]))))

;; 2016.11.25
(defsound test option-d
  (+ (let [freq (switch (lf-pulse (dt:kr 1 [2 2 4 1]))
                        (stepper (impulse 1/4) 0 400 800 200)
                        (lin-lin (m-map lf-saw [1/3 1/2 1/7 3/2]) 0 1 100 1800))]
       (sin-osc freq))
     (let [gate (impulse (dt:kr 2 [1 1 2 1]) (dt:kr 2 [1/2 1/2 0 1/2]))
           freq (dt:kr 2 [4800 4800 3200.0])]
       (tanh (* (rhpf (white-noise) freq 0.3) (env-gen (env-perc 0.05 0.1) gate))))))

;; 2016.11.26
(defsound test option-d
  (let [gate (impulse 4)
        freq (switch (lf-pulse 4)
                     (latch:ar (lin-lin (lf-saw 1/6) 0 1 400 500) gate)
                     (latch:ar (lin-lin (m-map lf-tri [3 4 1/5]) 0 1 300 900) gate))]
    (+ (sin-osc freq)
       (let [gate (pattern (impulse 8) [[1 1 1 0 1 0]
                                        [1 1 1 0 1 0]
                                        [0 0 0 0 0 0]])]
         (* (pink-noise) (env-gen (env-perc 0.01 0.07) gate)))
       (let [freq (rg-lin (lf-tri 1/8 -1) 100 1200)]
         (tanh (rlpf (+ (saw freq) (lf-tri (* 4 freq)))
                     (sin-r 1/5 500 2400)))))))

;; 2016.11.28
(defsound test option-d
  (let [gate (switch (lf-pulse 1/4)
                     (pattern (impulse 8) [1 1 1 1 0 0 1 1 0 1 0 0])
                     (pattern (impulse 8) [0 1 0 1 0 0 1 0 0 0 1 0]))
        gate2 (switch (lf-pulse 1/9)
                      (pattern (impulse 3) [1 1 1 0 0 1 0 1 0])
                      (pattern (impulse 5) [1 1 1 0 0 1 0 1 0]))]
    (+ (* (rlpf (pink-noise) (env-gen (envelope [0 10000 500] [0 0.01] EXP) gate) 0.1)
          (env-gen (env-perc 0 0.08) gate))
       (* (rlpf (brown-noise) 300 0.1) (env-gen (env-perc 0.1 0.3) gate2))
       (let [freq [400 680.0 820 2101.5703125 3078.5332035 9235.599]]
         (m-map #(* (pulse %) (sin-r 3.2 0.8 1) (clip:ar (sin-r 1/16 -0.2 1) 0 1)) freq)))))

;; 2016.11.29
(defsound test option
  (+ (let [gate (impulse (dt:kr 1/2 [4 8 4 8 4 8 1 1]))
           freqs (take 15 (iterate #(* 1.2 %) 180))]
       (pan2 (* 10 (mix (map #(* (/ 1 %) (sin-osc (* (dt:kr 1/4 [1 2.5 1 2.5 1 2.5 1 1.5]) %2)))
                             (iterate inc 1) freqs))
                (env-gen (env-perc 0.05 0.1) gate))))
     (detune 100 (fn [detune]
                   (let [gate (impulse 1/4)
                         env (env-gen (envelope [0 0 1 0.5 0] [0 3 0.8 0.2] EXP) gate)
                         freq (* (+ detune
                                    (sin-r 2/3 100 800)
                                    (env-gen (envelope [0 400 4000 400 0] [0 2.95 0.2 0.4] EXP) gate))
                                 (sin-r 200 0.5 1))
                         r-freq (env-gen (envelope [0 400 12000 400 0] [0 3 0.8 0.2] EXP) gate)]
                     (* env (rlpf (saw freq) r-freq)))))))

;; 2016.12.01
(defsound test option-d
  (let [arr [1/3 1/5 1/8 1.2]
        gate (m-map impulse arr)
        freq (* dr (lin-exp (clip:ar (m-map sin-osc arr) -0.5 0.5) -0.5 0.5 100 9000))
        attack (latch (sin-r:kr 3 0 0.3) gate)
        release (latch (sin-r:kr 2.7 0 0.2) gate)
        env (env-gen (env-perc attack release) gate)]
    (+ (sin-osc freq)
       (+ (* (rlpf (saw (/ freq 8)) freq 0.05) env)
          (* (pink-noise) env)))))
;; 2016.12.02
(defsound test option-d
  (+ (m-map #(* (sin-osc %)
                (env-gen (env-perc 0.01 (rand 0.2)) (impulse 1 (rand 1))))
            (take 20 (iterate #(* % (+ 1 (rand 0.2))) 300)))
     (let [gate (impulse 1)
           max-freq (select:kr (dq:kr gate [0 0 0 1 0 0 2 1])
                               [(env-gen (envelope [0 200 500] [0 0.08]) gate)
                                (env-gen (envelope [0 200 250] [0 0.08]) gate)
                                (env-gen (envelope [0 800 250] [0 0.15]) gate)])
           freq (rg-lin (white-noise) 200 max-freq)]
       (* (sin-osc freq) (env-gen (env-perc 0.18 0.2) gate)))))

;; 2016.12.03
(defsound test option
  (* 8
     (let [ratio (lin-lin (m-map lf-saw [1 0.7 1/3]) 0 1 1 2)]
       (+ (splay (map #(let [gate (impulse 1 (rand 1))
                             f-env (* ratio (env-gen (envelope [0 %1 %1 %2] [0 0.15 0.08]) gate))
                             env (env-gen (env-perc 0.05 0.5) gate)]
                         (* (sin-osc f-env) env))
                      (take 8 (iterate #(+ 180 %) 200))
                      (take 8 (iterate #(+ 350 %) 500))))
          (* 1/100 (saw (* (- 2 ratio) 1800)))))))

;; 2016.12.04
(defsound test option-d
  (let [freq (rg-lin (m-map lf-tri [1/4 1/3 1/7 2.3]) 800 2500)
        freq2 (rg-lin (m-map lf-tri [1/4 1/3 1/7 2.3]) 20 180)]
    (free-verb (+ (* 1/3 (pulse (* (rg-lin (lf-noise0 10) 0.8 1) freq)))
                  (* 4 (saw freq2) (lf-pulse 8)))
               0.8 0.1)))

;; 2016.12.05
(defsound test option-d
  (+ (let [base (switch (lf-pulse 1/4)
                        (m-map lf-pulse [1/3 1/7 0.3])
                        (m-map lf-tri [2 3.3 1.5]))
           freq (lin-exp base 0 1 200 1800)]
       (sin-osc freq))
     (switch (lf-pulse 1/4)
             (let [gate (impulse 1 1/2)
                   freq (env-gen (envelope [0 10000 100] [0 0.1]) gate)]
               (tanh (* 38 (m-map lf-tri [23 52 10 45]) (rlpf (brown-noise) freq)
                        (env-gen (env-perc 0.04 0.3) gate))))
             (let [gate (impulse (dt:kr 1 [4 8]))
                   freq (env-gen (envelope [0 10000 100] [0 0.01]) gate)]
               (* (pulse freq) (env-gen (env-perc 0.001 0.05) gate))))))

;; 2016.12.07
(defsound test option-d
  (+ (let [gate (m-map impulse [1.5 2 3 7])
           f-env (env-gen (envelope [0 100 1] [0 0.01] EXP) gate)
           freq (* (+ (sin-r 3.3 0 50) (dq:kr gate [300 100])) f-env)]
       (* (rlpf (white-noise) freq 0.1) (env-gen (env-perc 0.008 0.01) gate)))
     (let [gate (m-map impulse [2 1.5])
           freq2 (* (dt 1 [400 400 400 400 400 400
                           1200 1000 1000 800 600 3200]) (env-gen (envelope [0 8 1] [0 0.01]) gate))]
       (* (sin-osc freq2) (env-gen (env-perc 0.02 0.08) gate)))))

;; 2016.12.08
(defsound test option-d
  (let [arr (take 16 (iterate #(+ 200 %) 400))
        gate (impulse 12)
        gate2 (m-map impulse [1/5 1/3 1/7 1/8 4/3])
        ratio (rg-lin (lf-pulse 1 0 7/8) 1 1.3)
        env (env-gen (env-perc 0.15 1.5) gate2)
        freq (* ratio (stepper gate gate2 200 4000 200 200))]
    (+ (* (sin-r 0.7 0.2 1) (sin-osc freq))
       (* (rlpf (* (m-map saw [400 3800 180]) (brown-noise)) freq 0.1) env))))

;; 2016.12.09
(defsound test option-d
  (let [vib (* (sin-r 1.3 0 1) (sin-r 8.3 0.7 1))]
    (+ (let [freq (lin-lin (m-map #(clip:ar (sin-osc %) 0 0.5) [0.2 0.3 0.7 1.8])
                           0 0.5
                           100 1000)]
         (* vib (m-map #(* %2 (lf-tri (* freq %))) [1 1.8 8.82] [1 1/4 1/8])))
       (let [freq (dt 1/2 [500 500 600 600 700 700 800 900 1400 1800])
             freqs (take 10 (iterate #(* % 1.3) freq))
             gate (lf-pulse (dt:kr 1 [1 2 4 12]) 0
                            (dt:kr 1 [3/4 3/4 1/4 1/8]))
             env (env-gen (asr 0.01 0.8 0.05) gate)]
         (* vib (m-map sin-osc freqs) env))
       (let [freq (* 5000 (sin-r 0.9 1 1.6))]
         (* (switch (lf-pulse 1/4)
                    (delay-n vib 0.1 0.1)
                    (lf-pulse 6.5))
            (rhpf (brown-noise) freq))))))

;; 2016.12.10
(defsound test option-d
  (m-map (fn [x] (let [gate (impulse x)
                       freq (latch:ar (rg-lin (sin-osc 1/5) 400 (dt:kr 1 [2400 6000])) gate)
                       vol (rg-lin (m-map lf-tri [2 1/7 1/8 1/2]) 0.5 1)
                       gate2 (impulse 1/2)]
                   (+ (* vol (sin-osc freq) (env-gen (env-perc 0.01 0.18) gate))
                      (* (m-map #(rlpf (saw (sin-r 3.2 freq (* 1.02 freq)))
                                       (* % (env-gen (envelope [0 1 0] [0 1]) gate2)))
                                [1000 3500 8200])
                         (env-gen (envelope [0 1 1 0] [0.2 0.3 0.5]) gate2))
                      (* (rlpf (white-noise) (dq gate [8000 3000]) 0.1)
                         (env-gen (env-perc 0.01 0.1)
                                  (* (lf-pulse (dt:kr 2 [1/2 1/4])) gate))))))
         [12 8]))

;; 2016.12.16
(defsound test option-d
  (let [gate (impulse 8)
        freq (* dr (dq gate [500 500 300 300]))]
    (+ (* (rlpf (white-noise) freq) (env-gen (env-perc 0.01 0.001) gate))
       (* (lf-pulse 1/4) (lf-pulse 8) 1/4 (+ (sin-osc 800)
                                             (* 1/8 (lf-pulse 1/8 1/2) (pulse 2400))
                                             (* 1/16 (lf-pulse 1/3 1/3) (saw 4800)))))))

;; 2016.12.17
(defsound test option-d
  (let [freq 440
        some-freq (env-gen (envelope [1/2 1/2 8 500 1/2] [6 4 4 2]) :action FREE)
        width (env-gen (envelope [1 1 0.01 1] [9 4 3]) :action FREE)
        gate (impulse some-freq)]
    (+ (tanh (* (clip:ar (sin-osc some-freq)
                         0.25 1)
                (+ (sin-osc freq)
                   (pulse (* 4 freq) (sin-r 2.8 0.5 0.6)))))
       (* (lf-pulse 16 0 width)
          (gray-noise) (env-gen (env-perc (clip (/ 1 some-freq) 0.01 0.5) 0.1) gate)))))

;; 2016.12.18
(defsound test option-d
  (m-map #(let [r-freq %1
                gate (impulse r-freq)
                length (dq gate %2)
                env (* (env-gen (env-perc 0.01 (/ 1 r-freq 2)) gate) (lf-pulse r-freq 0 length))]
            (+ (let [freq (dq:kr gate [1800 300])]
                 (* (rlpf (brown-noise) freq 0.1) env))
               (let [freq (dq:kr gate (take 10 (iterate (fn [x] (* 1.2 x)) 300)))]
                 (* (sin-osc freq) (env-gen (env-perc 0.02 0.18) gate)))))
         [(dt:kr 4 [1 1 2 4])
          (dt:kr 1 [1 1 2 4])]
         [[1/4 1/8]
          [1/8 1/2]]))

;; 2016.12.19
(defsound test option-d
  (let [freq (lin-lin (m-map lf-pulse [1/2 4 9]) 0 1 500 2500)]
    (switch (lf-pulse (line:kr 1/8 2 16) 0
                      (lin-lin (m-map lf-tri:kr [1/3 1/4 2 1/7])
                               -1 1 0 1))
            (rlpf (switch (lf-saw 8) (pink-noise) (brown-noise))
                  freq 0.1)
            (free-verb (* (sin-osc freq)
                          (env-gen (env-perc 0.01 0.08) (impulse 8)))))))

;; 2016.12.21(queen)
(defsound test option-d
  (let [speed 2.7
        gate (* (impulse speed) (lf-pulse (/ speed 4) 0 3/4))
        f-env  (env-gen (envelope [0 8 1] [0 0.008]) gate)
        freq (* dr (demand gate 0 (dseq [300 300 1500] INF)))]
    (* 2 (rlpf (gray-noise) (* freq f-env)) (env-gen (env-perc 0.03 0.42) gate))))

;; 2016.12.28
(defsound test option-d
  (switch (lf-pulse 1/2 0 (dt:kr 2 [3/4 3/4 3/4 3/4 1/2 1/2 1/4 1/6]))
          (* (saw (lin-exp (sin-osc 100) -1 1 10 2000))
             (saw 200))
          (+ (* (saw (sin-r 1.3 100 10000))
                (m-map sin-osc [800 870 930]))
             (* (rlpf (white-noise) (rg-exp (sin-osc 1.5) 100 10000) 0.06)
                (env-gen (env-perc 0.01 0) (dust:kr 40))))))

;; 2016.12.28
(defsound test option-d
  (let [gate (pattern (impulse 8) [1 0 0 0 1 1 0 0
                                   1 1 1 1 0 0 0 0
                                   1 0 0 1 1 1 1 1])
        freq (rg-exp (latch:ar (white-noise) gate) 1800 8000)
        ratio (round (rg-lin (lf-noise1 4) 1.5 4) 0.5)]
    (+ (* (m-map sin-osc [freq (* ratio freq) (* ratio ratio freq)]))
       (* (line 0.1 1 8)
          (tanh (ringz (* (env-gen (env-perc 0.05 0.5) gate) (brown-noise))
                       (* freq 1/2) 0.1))))))

;; 2016.12.29
(defsound test option-d
  (m-map #(let [freq (latch:ar (rg-exp (sin-osc %) 80 4000) (impulse 7 %2))]
            (* (line 0 1 %3) (sin-osc freq)))
         [1/5 1/4 7.3 1/16]
         [0 1/4 3/4 0]
         [0.5 12 4 8]))

;; 2016.12.30 (like world)
(defsound test option-d
  (+ (m-map #(let [gate (impulse 1/6 %)
                   vol (rg-exp (latch:ar (white-noise) gate) 8 32)]
               (* vol (rlpf (white-noise) 100)
                  (sin-osc %2) (env-gen (env-perc 0.05 2) gate)))
            (take 8 (iterate #(mod (* 1.3 %) 1.0) 0.2))
            (cycle [4400 8900 1200 2000 5100 2800]))
     (let [vol (clip:ar (lf-saw 1/3 1) 0 1)
           freq-vol (lf-saw (dt:kr 3 [1 2 -1 4 -8 1/3]) 1)
           freq1 (lin-exp freq-vol 0 1 40 1200)
           freq2 (lin-exp freq-vol 0 1 220 5400)]
       (* vol vol (saw freq1) (sin-osc (* freq2 (sin-r 80.2 0.5 1.5)))))))

;; 2016.12.30
(defsound test option-d
  (+ (let [gate (impulse (dt:kr 1 [1 1 4 12]))
           max-freq (dt 4 [1200 1800 600])
           freq (latch:ar (lin-exp (m-map lf-pulse [1 4.4 1/3 1/4 1/2]) -1 1 400 max-freq) gate)]
       (m-map #(* 4 (/ 1 % %5) (env-gen (envelope [0 1 1 0] [%2 %3 %4]) gate)
                  (sin-osc (* % freq)))
              (take 12 (iterate #(+ 1 (* % 2)) 1))
              (cycle [0.10 0.01 0.15])
              (cycle [0.07 0.08 0.01])
              (cycle [0.05 0.08 0.28])
              (cycle [1 0.8])))
     (let [gate (impulse 1/4 0.2)]
       (* (pink-noise)
          (lf-pulse (dt:kr 1 [8 8 8 16]) 0 1/8)
          (env-gen (envelope [1e-8 1 1e-12] [2 0.5] EXP) gate)))
     (let [gate (impulse 1/4 1e-16)
           f-env (env-gen (envelope [0 8 1] [0 0.05]) gate)]
       (* (lf-tri (* f-env 150)) (brown-noise) (env-gen (env-perc 0.08 2) gate)))))

;; 2016.12.31
(defsound test option-d
  (let [max (sin-r 1/2 0.1 1)
        base (m-map #(clip:ar (lf-saw %) 0 max) [1/3 1.8 0.272])
        gate (a2k (< base 0.5))
        freq (lin-exp base 0 1 200 2500)
        env (* (sin-osc 3.2) (sin-r 2.9 0.5 1))]
    (+ (* (cubed (sin-r 1/4 0.1 1)) (tanh (g-verb (* env (sin-osc freq)))))
       (* (rlpf (brown-noise) (* 3 freq) 0.05)
          (env-gen (env-perc 0.05 1) gate))
       (let [gate (pattern (impulse (* 1.8 4))
                           [[1 0 0 0]
                            [1 0 0 0]
                            [1 0 0 0]
                            [1 0 1 0]])
             f-env (env-gen (envelope [0 8 1] [0.002]) gate)]
         (* (tanh (rlpf (white-noise) (* freq f-env 1/4) 0.01))
            (env-gen (env-perc 0.001 0.1) gate))))))

(defsound test option-d
  (let [base (sin-osc 1/8)
        vol (lin-exp base -1 1 1 0.01)
        freq (lin-exp base -1 1 100 1000)]
    (* vol (sin-osc freq))))

