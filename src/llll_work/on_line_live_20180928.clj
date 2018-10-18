(ns llll-work.on-line-live-20180928
  (require [llll.core :as l4]
           [llll.clay.clay :as cl]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

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

(defsound ping option
  (let [gate (impulse 1/4 [0 1/2])
        freq (* [100 150] (stepper gate 1 1 8))]
    (* (sin-osc freq)
       (env-gen (env-perc 0.05 0.5) gate)
       )))


(defcgen pan-gau [pos {:default 0}
                  in {:default 0}]
  (:ar (let [x (squared pos)
             a (exp (* -14 x))
             b (exp (* -8 x))]
         (* in [(select (not-pos? pos) [a b])
                (select (not-pos? pos) [b a])]))))

(defsound flow option
  (let [freq (latch:ar (u/sin-r 0.3 440 2200) (impulse 1/7))]
    (pan-gau (u/sin-r 0.06 -2 2) (sin-osc freq))))

(defsound strong-flow option
  (let [pos (sin-osc 0.05)
        car-freq (u/rg-exp (lf-noise0 (u/rg-exp (sin-osc 0.1) 0.001 16)) 50 800)
        freq (u/rg-exp (sin-osc 0.1) 8200 8210)]
    (-> (pan-gau pos (sin-osc freq (u/rg-lin (sin-osc car-freq) 0 (* 2 Math/PI))))
        (free-verb 0.5 0.1))))


(defsound chill option-d
  (let [snd (bpf (white-noise) (* dr [1000 2000 10000]) (u/rg-exp (lf-saw -1/10) 1 0.01))]
    (-> snd
        (free-verb 1 0.1)
        (comb-l 1/8 1/8 1.2)
        (* 10)
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

(defcgen pan-exp [pos {:default 0}
                in {:default 0}]
  (:ar (let [x (abs pos)
             a (exp (* -14 x))
             b (exp (* -8 x))]
         (* in [(select (not-pos? pos) [a b])
                (select (not-pos? pos) [b a])]))))

(defsound nami option
  (rotate2 (rlpf (white-noise) (u/rg-exp (sin-osc 0.057) 200 1800))
           (rlpf (white-noise) (u/rg-exp (sin-osc 0.03) 100 1000))
           (sin-osc (u/rg-exp (sin-osc 0.008) 0.03 1))))

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

