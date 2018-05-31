(ns llll-work.core
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(kill-server)

(recording-start (str "/home/maeda/code/clojure/llillill/record/"
                      (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (java.util.Date.))
                      ".wav"))

(l4/initialize {})

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

(def oe-buf (load-sample "~/Dropbox/flower_in_the_winter_snow.aif"))

(def oe-frame-num (buf-frames oe-buf))
(defn -t [m s sss]
  (+ (* 60 m)
     s
     (/ sss 1000)))

(defmacro oe [ratio from to]
  (let [total (-t 1 35 0)]
    `(let [freq# (~'* ~ratio (/ 1/2 (~'- ~to ~from)))]
       (buf-rd 2 oe-buf
               (~'*
                (u/rg-lin (lf-saw freq# 1) (~'/ ~from ~total) (~'/ ~to ~total))
                (buf-frames oe-buf))))))

(l4/control :guiter :vol {:dur 4 :to 1})

(defsound guiter
  {:period 32
   :swap-option {:switch-dur 4}
   :synth-option {:type :detune}}
  (sin-osc (u/rg-lin (sin-osc (* dr 3.3)) 440 1792)))

(defsound guiter
  {:period 32
   :swap-option {:switch-dur 4}
   :synth-option {:type :detune}}
  (let [snd  (oe 1 (-t 0 35 835) (-t 1 35 0))]
     (-> snd
         (* 64)
         (* (lag (lf-pulse 8 0 1/8) (u/rg-lin (sin-osc 0.3)  0.08 0.1)))
         (* (lf-pulse 4 0 7/8))
         (resonz (u/rg-exp (lf-tri 0.1 dr) 800 10000))
         (distort)
         (u/reduce-> (fn [acc x] (free-verb acc (u/rg-lin (sin-osc 0.3)  x (* x 3)))) [0.03 0.2 0.01])
         (hpf 1500))))

(defsound guiter
  {:period 32
   :swap-option {:fade-out-dur 32}}
  (let [snd (oe 1 (-t 0 35 835) (-t 1 35 0))
        freq (pitch snd)
        ifreq (/ 1 freq)
        ]
    (->  snd
         (* 8 (sin-osc (* 4 freq)) (sin-osc (* 8 freq)))
         (* (lf-noise0 8))
         (u/reduce-> (fn [acc x] (comb-l acc (* ifreq x) (* ifreq x) 0.2)) [1/8 2 1])
         tanh)))

(defsound guiter
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :period 32
   :swap-option {:switch-dur 32}}
  (let [snd (oe 1 (-t 0 35 835) (-t 1 35 0))
        p (pitch snd)]
    (u/switch :-mix snd
              (->  snd
                   (u/switch-> (u/rg-lin (lf-tri (/ 1 dur)) 0 1)
                               (u/reduce-> (fn [acc x] (sin-osc (* acc x :-freq)))
                                           [(!! (if b0 1/8 4))
                                            (!! (if b1 1/2 2))
                                            (!! (if b2 1/4 3/2))]))
                   (ringz (!! (* 100 (+ c3 (inc c1)))) :-ringz)
                   (u/reduce-> (fn [acc x] (free-verb acc x 1)) [0.01 0.01])))))

(l4/control :guiter :-freq {:dur 16 :to 10})

(l4/control :guiter :vol {:dur 16 :to 1})
(l4/dump)
(l4/stop :guiter)

llll.engine.engine/%lines




(demo 12 (let [N 7
               T (u/rg-exp (u/m-map lf-pulse [0.3 0.54 0.8]) 0.00001 0.0025)
               M 0.016
               gate (impulse (/ 1 (+ (* N T) M)))
               env (env-gen (envelope [0 1 1e-4] [0 (* N T)] :exp) gate)
               snd (squared (sin-osc (/ 1 T)))]
           (-> (* snd env)
               (free-verb 0.8 (x-line 0.1 10)))))

(demo 12 (let [freq 440]
           (reduce (fn [acc x] (sin-osc (* x freq acc)))
                   (sin-osc freq)
                   [1/2 1/4 (u/sin-r 0.3 1/2 8) 9/4])))

(demo 12 (let [freq 440]
           (reduce (fn [acc [m1 m2]]
                     (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
                   (sin-osc freq)
                   [[1 2] [1/2 7] [1/4 7]])))

(demo 12 (let [gate (impulse 1)
               gate2 (impulse 1/4 5/8)
               freq 60
               f-env (env-gen (envelope [0 2.5 1] [1e-5 0.04]) gate)]
           (+ (tanh (* 1.2 (sin-osc (* freq f-env))
                       (env-gen (envelope [0 1 0.5 0] [0 0.25 0.01]) gate)))
              (* 1/2 (tanh (* 8 (sin-osc (* freq f-env ))
                              (env-gen (envelope [0 1 0.5 0] [0 0.125 0.08]) gate2))))
              (* (impulse 8)
                 (lf-pulse 1/4 1/2 0.5))
              (* (impulse 2)
                 (lf-pulse 1/6 1/4 0.25)))))

(demo 12 (+ (splay (-> (map (fn [freq1 freq2 i] (* (/ 1 i) (rlpf (lf-noise1 freq1) freq2 0.01)))
                            (u/n-range 100 600 8)
                            (shuffle (u/n-range 20 100 8))
                            (iterate inc 1))
                       (* 8)
                       tanh
                       (* (env-gen (env-perc 0.05 2) (impulse 1/2)))))
            (let [gate (impulse 2)
                  f-env (env-gen (envelope [0 3 1] [0.001 0.01]) gate)
                  freq 60]
              (-> (env-gen (env-perc 0.05 1) gate)
                  (* (sin-osc (* freq f-env)))))))

(demo 12 (splay (map #(let [f-env (env-gen (envelope [0 4 1] [0.005 0.01]) (impulse 1 %3))]
                        (* 8 (sin-osc (* %2 f-env (u/rg-lin (lf-pulse % 0 0.1) 40 50)))))
                     (u/n-range 1 2 8)
                     (u/n-range 1 4 8)
                     (shuffle (u/n-range 0 1 8)))))





