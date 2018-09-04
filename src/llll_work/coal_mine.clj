(ns llll-work.coal_mine
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(kill-server)

(do
  (recording-start (str "/home/maeda/code/clojure/llll-work/record/"
                        (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (java.util.Date.))
                        ".wav"))

  (l4/control :work :-room {:set 1})
  (l4/control :drill :-freq {:set 200})
  (l4/control :work :vol {:dur 1024 :to 0.15})
  (l4/control :bass  :vol {:dur 512 :from 0 :to 1.0})
  (l4/control :bass2 :vol {:dur 1024 :to 1.0})
  (l4/control :bass3 :vol {:dur 1024 :from 1.0 :to 0})

  (l4/control :collapse :vol {:dur 128 :to 0.0625}))


(do
  (l4/control :work :vol {:set 0})
  (l4/control :bass  :vol {:set 0})
  (l4/control :bass2 :vol {:set 0})
  (l4/control :bass3 :vol {:set 0})
  (l4/control :collapse :vol {:set 0}))
(recording-stop)

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


(defsound tracker
    {:swap-option {:switch-fade-in 0
                   :switch-fade-out 8
                   :fade-out-dur 32}
     :state {:initial (update-state {:count 0})
             :update update-state}
     :period 32}
    (splay
     (map (fn [count ratio phase]
            (let [gate2 (impulse (* :f4 count) )]
              (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio)
                                                             [0.1 50 10 1 100])))
                   (* (u/sin-r (* :f8 ratio) 128 5120)
                      (u/dq gate2 [1 1/2 1/4])
                      (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                   ;; (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t2])
                   ;; (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.7 0.8 4.3])
                   ;;                  (* ratio 500) (* ratio 2000)) 0.001)
                   ;; (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [0.002])
                   (tanh)
                   (* 2)
                   )))
          [1/2 2/3]
          [5/4 4]
          (u/n-range 0 1 6))))



(defsound tracker
  {:swap-option {:switch-fade-in 0
                 :switch-fade-out 8
                 :fade-out-dur 32}
   :state {:initial (update-state {:count 0})
           :update update-state}
   :period 32}
  (splay
   (map (fn [count ratio phase]
          (let [gate2 (impulse (* :f4 count) )]
            (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio)
                                                           [0.1 50 10 1 100])))
                 (* (u/sin-r (* :f8 ratio) 128 8960)
                    (u/dq gate2 [1 1/2 1/4])
                    (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                 (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t2 :t3 :t1])
                 (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.7 0.8 4.3])
                                  (* ratio 500) (* ratio 2000)) 0.003)
                 (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [0.004 0.02 0.01])
                 (tanh)
                 (* 2)
                 )))
        [4 2 3 9/2 1 8]
        [5/4 4 3/2 2 1 1/2]
        (u/n-range 0 1 6))))

(l4/stop :tracker)

(defsound drill
  {:period 16
   :synth-option {:type :detune}
   :state {:initial (update-state {:count 0})
           :update update-state}
   :swap-option {:fade-in-dur 8
                 :switch-dur 4
                 :fade-out-dur 32}}
  (let [freq (!! (if b3 (* (if (= (mod c0 4) 0) 3 1) 440) 0))]
    (-> (sin-osc (* dr freq))
        (u/reduce-> (fn [acc [m1 m2]]
                      (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
                    [[1 2] [1/2 5/2] [1/4 5]])
        (u/switch-> (u/sin-r (/ 1 dur) 0 1)
                    (ringz (* (u/rg-exp (lf-saw (!! (if b2 0.28 0.037))) 1/4 2) freq) 0.002))
        (u/reduce-> (fn [acc [mix room]] (free-verb acc mix room))
                    [[(!! (if b2 0.1 0)) 0.1]
                     [0 0.8]])
        (* 8)
        distort)))

(defsound drill
  {:period 16
   :synth-option {:type :detune}
   :state {:initial (update-state {:count 0})
           :update update-state}
   :swap-option {:fade-in-dur 8
                 :switch-dur 4
                 :fade-out-dur 32}}
  (let [freq (!! (if b3 (* (if (= (mod c0 4) 0) 3 1) 440) 100))]
    (-> (sin-osc (* dr freq))
        (u/reduce-> (fn [acc [m1 m2]]
                      (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
                    [[1 2] [1/2 (!! (if b1 7 2))] [1/4 (!! (if b2 7 5))]])
        (u/switch-> (u/sin-r (/ 1 dur) 0 1)
                    (ringz (* (u/rg-exp (lf-saw (!! (if b2 0.28 0.037))) 1/4 2) freq) 0.002))
        (u/reduce-> (fn [acc [mix room]] (free-verb acc mix room))
                    [[(!! (if b2 0.1 0)) 0.1]
                     [0 0.8]])
        (* 8)
        distort)))

(defsound work
  {:period 32
   :synth-option {:type :detune}
   :swap-option {:fade-in-dur 8
                 :switch-dur 4
                 :fade-out-dur 32}}
  (let [N 7
        T (u/rg-exp (u/m-map lf-pulse [0.3 0.54 0.8]) 0.00001 0.0025)
        M 0.016
        gate (impulse (/ 1 (+ (* N T) M)))
        env (env-gen (envelope [0 1 1e-4] [0 (* N T)] :exp) gate)
        snd (squared (sin-osc (/ dr T)))]
    (-> (* snd env)
        (free-verb 0.8 :-room))))

(defsound collapse
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  (splay (-> (map (fn [freq1 freq2 i] (* (/ dr i) (rlpf (lf-noise1 freq1) freq2 0.01)))
                  (u/n-range 100 600 8)
                  (shuffle (u/n-range 20 100 8))
                  (iterate inc 1))
             (* 2)
             (u/switch-> (u/rg-lin (lf-tri (* :f1 (!! (if b0 32 4)))) 0 1)
                         (u/reduce-> (fn [acc x] (free-verb acc x 1)) [1 1 1]))
             tanh
             (* (env-gen (env-perc 0.05 2) (* :-gate
                                              (lf-pulse (!! (if (= (mod c2 4) 0) 1/2 1/8)) 0
                                                        (!! (if b3 1/8 1/4)))))))))

(defsound bass
  {:period 32
   :synth-option {:type :detune}
   :swap-option {:fade-in-dur 4
                 :switch-dur 1
                 :fade-out-dur 32}}
  (let [gate (u/throttle :-gate 6)]
    (+
     (let [f-env (env-gen (envelope [0 3 1] [0.001 0.01]) gate)
           freq 80]
       (-> (env-gen (env-perc 0.05 1) gate)
           (* (sin-osc (* freq dr f-env)))
           (free-verb 1 1)))
     (let [freq 58
           f-env (env-gen (envelope [0 2.5 1] [1e-5 0.04]) gate)]
       (tanh (* 1.2 (sin-osc (* dr freq f-env))
                (env-gen (envelope [0 1 0.5 0] [0 0.25 0.01]) gate)))))))




(defsound bass
  {:period 32
   :synth-option {:type :detune}
   :swap-option {:fade-in-dur 4
                 :switch-dur 1
                 :fade-out-dur 32}}
  (let [gate (u/throttle :-gate (u/rg-lin (lf-noise0:kr 1) 1 6))]
    (+ (let [f-env (env-gen (envelope [0 3 1] [0.001 0.01]) gate)
             freq 80]
         (-> (env-gen (env-perc 0.05 1) gate)
             (* (sin-osc (* freq dr f-env)))
             (free-verb 1 1)))
       (let [freq 58
             f-env (env-gen (envelope [0 2.5 1] [1e-5 0.04]) gate)]
         (tanh (* 1.2 (sin-osc (* dr freq f-env))
                  (env-gen (envelope [0 1 0.5 0] [0 0.25 0.01]) gate)))))))






(defsound bass2
  {:period 32
   :swap-option {:fade-in-dur 8
                 :switch-dur 4
                 :fade-out-dur 32}}
  (splay (map #(let [f-env (env-gen (envelope [0 4 1] [0.005 0.01]) (impulse 1 %3))]
                 (* 8 (sin-osc (* %2 f-env (u/rg-lin (lf-pulse % 0 0.1) 40 50)))))
              (u/n-range 1 2 8)
              (u/n-range 1 4 8)
              (shuffle (u/n-range 0 1 8)))))

(defsound bass3
      {:period 16
       :synth-option {:type :detune}
       :state {:initial (update-state {:count 0})
               :update update-state}}
      (-> (* (!! (if (= (mod c1 4) 0) 4 2 ))
             (sin-osc 80) (lf-tri (+ (!! (if b0 -10 -20)) 50))
             (env-gen (env-perc :t8 (* (!! (if b2 4 1)) :t16)) (impulse (* 1/8 :f16))))
          (lpf (u/rg-lin (lf-saw (* 1/4 :f16) -1) 200 800))
          tanh))






















;; ---------------------------

(comment
  (defsound beats
    {:swap-option {:switch-fade-in 0
                   :switch-fade-out 4
                   :fade-out-dur 32}
     :state {:initial (update-state {:count 0})
             :update update-state}
     :period 32}
    (splay
     (map (fn [count ratio]
            (let [gate2 (impulse (* :f4 count))]
              (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio)
                                                             [0.1 50 10 1 100])))
                   (* (u/sin-r (* :f8 ratio) 128 :-max-ratio)
                      (u/dq gate2 [1 1/2 1/4])
                      (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                   ;; (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t2 :t1])
                   ;; (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.7 0.8 4.3])
                   ;;                  (* ratio 500) (* ratio 2000)) :-ringz-delay)
                   ;; (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [0.002 :-mix2 0.01])
                   (tanh)
                   (* 2)
                   )))
          [1/2 2/3]
          [5/4 4])))

  (l4/control :beats :-ringz-delay {:dur 128 :to 0.003})


  (defsound beats
    {:swap-option {:switch-fade-in 0
                   :switch-fade-out 4
                   :fade-out-dur 32}
     :state {:initial (update-state {:count 0})
             :update update-state}
     :period 32}
    (splay
     (map (fn [count ratio phase]
            (let [gate2 (impulse (* :f8 count))]
              (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio)
                                                             [0.1 50 10 1 100])))
                   (* (u/sin-r (* :f8 ratio) 128 5120)
                      (u/dq gate2 [1 1/2 1/4])
                      (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                   (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t1 :t3 :t2])
                   (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.7 0.8 4.3])
                                    (* ratio 500)
                                    (* ratio 2000)) 0.002)
                   (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [0.001 0.02 0.01])
                   (tanh)
                   (* 4))))
          [4 2 3 1 8 4]
          [5/4 4 1 1/2 2/3 3/2]
          (u/n-range 0 1 6))))

  (defsound beats
    {:swap-option {:switch-fade-in 0
                   :switch-fade-out 4
                   :fade-out-dur 32}
     :state {:initial (update-state {:count 0})
             :update update-state}
     :period 32}
    (splay
     (map (fn [count ratio phase]
            (let [gate2 (impulse (* :f8 count))]
              (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio)
                                                             [0.1 50 10 1 100])))
                   (* (u/sin-r (* :f8 ratio) 512 4096)
                      (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                   (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t1 :t3])
                   (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.6 0.7 4.3])
                                    (* ratio 500) (* ratio 2000))
                          :-ring-decay)
                   (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [:-mix1 :-mix2 :-mix3])
                   (tanh)
                   (* 4))))
          [8 4 6 2 16 8]
          [2 4 1/2 3 1/2 3/2]
          (u/n-range 0 1 6))))

  (defsound beats
    {:swap-option {:switch-fade-in 0
                   :switch-fade-out 32
                   :fade-out-dur 32}
     :period 32}
    (splay
     (map (fn [count ratio phase]
            (let [gate2 (impulse (* :f4 count))]
              (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio) [0.1 50 10 1 100])))
                   (* (u/sin-r (* :f8) 128 4096) (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                   (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t1 :t3 :t2])
                   (ringz (u/rg-exp (u/m-map lf-pulse [0.8 0.7 0.3 4.3])
                                    (* ratio 500)
                                    (* ratio 4000))
                          0.004)
                   (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [:-mix1 :-mix2 :-mix3])
                   (tanh)
                   (* 4))))
          [4 2 1 3 8 4]
          [5/4 4 1 1/2 2/3 3/2]
          (u/n-range 0 1 6)))))

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


