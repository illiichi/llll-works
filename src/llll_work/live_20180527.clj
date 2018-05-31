(ns llll-work.core
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(kill-server)

(recording-start (str "/home/maeda/record/"
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


(defsound effect
  {:period 32
   :state {:initial (update-state {:count 1})
           :update update-state}
   :synth-option {:type :detune}
   :swap-option {:switch-dur 32}}
  (let [snd (sound-in)
        h-snd (hpf snd 950)]
    (-> h-snd
        (delay-c  :t8 :t8)
        (ringz 8000 0.1))))


(defsound rythm
  {:period 32}
  (let [gate (lf-pulse :f4)
        seed-freq (lin-lin gate 0 1 800 900)
        freq (lin-lin (u/m-map lf-pulse [1/10 0.34 0.25 2.5])
                      0 1
                      (* 4 seed-freq) (* 10 seed-freq))]
    (+ (splay
        (let [base-freq (map #(* % (u/dq gate [100 80])) [1 1.01])
              fenv (map (fn [count] (env-gen (envelope [0 8 1] [0.0001 0.01])
                                             (u/throttle gate count)))
                        [4 8 2])]
          (-> (* (sin-osc (* base-freq fenv)) (env-gen (env-perc 0.001 0.1) gate))
              (* (sin-osc (u/dq :-gate [1400 2400 400]))))))
       (splay (map (fn [x y]
                     (let [gate (impulse y)]
                       (-> (* 1/4 (lf-pulse 1/4) 75 (bpf (brown-noise) [2500 2510] 0.01) gate)
                           (ringz (u/m-map lf-pulse (map (fn [n] (* n x))
                                                         [400 480 320])) 0.1))))
                   [2 4 3/2]
                   [4 8 1/2])))))


(defsound thai
  {:period 32
   :swap-option {:fade-in-dur 128}
   :synth-option {:type :detune}}
  (let [gate (lf-pulse :f4)
        seed-freq (lin-lin gate 0 1 800 900)
        freq (u/rg-lin (u/m-map lf-pulse [1/10 0.34 0.25 2.5])
                       (* :-low seed-freq) (* :-high seed-freq))]
    (* 1/4 (sin-osc (reduce (fn [acc _] (* freq (sin-osc acc) dr))
                            seed-freq (range 3))))))


(defsound drill2
  {:period 32
   :synth-option {:type :detune}
   :state {:initial (update-state {:count 0})
           :update update-state}}
  (let [freq (!! (if b3 (* (if (= (mod c0 4) 0) 3 1) 440) 100))]
    (reduce (fn [acc [m1 m2]]
              (sin-osc (lin-exp acc -1 1 (* m1 freq) (* m2 freq))))
            (sin-osc freq)
            [[1 2] [1/2 (!! (if b1 4 2))] [1/2 (!! (if b2 7 5))]])))

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







