(ns llll-work.coal_mine
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

(l4/initialize {})
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

(defsound beats
  {:swap-option {:switch-fade-in 0
                 :switch-fade-out 4
                 :fade-out-dur 32}
   :state {:initial (update-state {:count 0})
           :update update-state}
   :period 32}
  (splay
   (map (fn [count ratio phase]
          (let [gate2 (impulse (* :f4 count :-speed))]
            (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio)
                                                           [0.1 50 10 1 100])))
                 (* (u/sin-r (* :f8 ratio) 32 :-max)
                    (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                 (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t2 :t1])
                 (ringz (u/rg-exp (u/m-map lf-pulse [0.8 0.3 0.1 (!! (if b0 40.3 0.43))])
                                  (* ratio 10) (* ratio (!! (if (= (mod c1 4) 0) 4000 2000))))
                        :-ring-deca)
                 (u/reduce-> (fn [acc x] (free-verb acc x 0.16)) [0.02 0.0003 0.002])
                 (tanh)
                 (* 2))))
        [4 8 12 2 4]
        [1/4 4 2/3 1/4 1/2]
        (u/n-range 0 1 6))))

(l4/control :beats :-ring-deca {:dur 128 :to 0.0016})

(l4/control :beats :-speed {:dur 128 :to 0.1})

(l4/stop :beats)


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
        (u/n-range 0 1 6))))

(l4/control :beats :vol {:dur 16 :to 1})

(l4/control :beats :-mix3 {:dur 128 :to 0.32})
(l4/control :beats :-mix2 {:dur 64 :to 0.08})
(l4/control :beats :-mix1 {:dur 128 :to 0.03})



(defsound beats
  {:switching-dur 2
   :period 32}
  (splay
   (map (fn [count ratio phase]
          (let [gate2 (impulse (* :f8 count))]
            (->  (saw (u/dq (u/throttle :-gate count) (map #(* % 100 ratio) [0.1 50 10 1 100])))
                 (* (u/sin-r (* :f8) 128 5120) (env-gen (envelope [0 1 1 0] [0 1e-7 1e-8]) gate2))
                 ;; (* (lf-pulse (/ 1 count) phase 1/4))
                 (u/reduce-> (fn [acc x] (+ acc (* 1/4 (delay-n acc x x)))) [:t1 :t3 :t2])
                 (ringz (u/rg-exp (u/m-map lf-pulse [0.8 0.7 0.3 4.3])
                                  (* ratio 500)
                                  (* ratio 4000))
                        0.003)
                 (u/reduce-> (fn [acc x] (free-verb acc x 0.01)) [0.001 0.02 0.01])
                 (tanh)
                 (* 4))))
        [4 2 3 1 8 4]
        [5/4 4 1 1/2 2/3 3/2]
        (u/n-range 0 1 6))))

(defsound bass
  {:period 32
   :synth-option {:type :detune}
   :swap-option {:fade-in-dur 8
                 :switch-dur 4
                 :fade-out-dur 32}}
  (+ (let [gate (u/throttle :-gate 6)
         f-env (env-gen (envelope [0 3 1] [0.001 0.01]) gate)
         freq 60]
     (-> (env-gen (env-perc 0.05 1) gate)
         (* (sin-osc (* freq f-env)))
         (free-verb 1 1)))
   (let [gate (u/throttle :-gate 4)
         freq 60
         f-env (env-gen (envelope [0 2.5 1] [1e-5 0.04]) gate)]
     (tanh (* 1.2 (sin-osc (* freq f-env))
              (env-gen (envelope [0 1 0.5 0] [0 0.25 0.01]) gate))))))

(do (l4/stop :bass)
    (l4/stop :beats)
    (l4/stop :bass2)
    (l4/stop :drill)
    (l4/stop :drill2)
    (l4/stop :jawa)
    (l4/stop :bass3))

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


(defsound drill
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
        snd (squared (sin-osc (/ 1 T)))]
    (-> (* snd env)
        (free-verb 0.8 (x-line 0.1 10)))))



(defsound drill2
  {:period 16
   :synth-option {:type :detune}
   :state {:initial (update-state {:count 0})
           :update update-state}}
  (let [freq (!! (if b3 (* (if (= (mod c0 4) 0) 3 1) 440) 100))]
    (reduce (fn [acc [m1 m2]]
              (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
            (sin-osc freq)
            [[1 2] [1/2 (!! (if b1 7 2))] [1/4 (!! (if b2 7 5))]])))

(l4/control :drill2 :vol {:dur 256 :to 0.125})

(defsound jawa
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :period 32}
  (splay (-> (map (fn [freq1 freq2 i] (* (/ 1 i) (rlpf (lf-noise1 freq1) freq2 0.01)))
                  (u/n-range 100 600 8)
                  (shuffle (u/n-range 20 100 8))
                  (iterate inc 1))
             (* 1)
             (u/switch-> (u/rg-lin (lf-tri (* :f1 (!! (if b0 32 4)))) 0 1)
                         (u/reduce-> (fn [acc x] (free-verb acc x 1)) [1 1 1]))
             tanh
             (* (env-gen (env-perc 0.05 2) (* :-gate
                                              (lf-pulse (!! (if (= (mod c2 4) 0) 1/2 1/8)) 0
                                                        (!! (if b3 1/8 1/4)))))))))

(l4/control :jawa :vol {:dur 16 :to 0})


(defsound bass3
  {:period 16
   :state {:initial (update-state {:count 0})
           :update update-state}}
  (-> (* (sin-osc 20) (lf-tri (+ (!! (if b0 10 -10)) 50))
         (env-gen (env-perc :t8 :t16) (impulse (* 1/4 :f16))))
      (lpf (u/rg-lin (lf-saw (* 1/4 :f16) -1) 200 800))))






(demo 12 (let [freq 440]
           (reduce (fn [acc x] (sin-osc (* x freq acc)))
                   (sin-osc freq)
                   [1/2 1/4 (u/sin-r 0.3 1/2 8) 9/4])))




