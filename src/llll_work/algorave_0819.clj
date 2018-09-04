(ns llll-work.algorave-0819
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(kill-server)

(l4/initialize {})
(l4/initialize {:osc-client false
                :osc-path "/hello"})
(l4/finish)

(recording-start (str "/home/maeda/code/clojure/llll-work/record/"
                      (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (java.util.Date.))
                      ".wav"))

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

(defn mm-mix [xss]
  (->> (u/transpose xss)
       (map mix)))

(defn mm-map [f & xss]
  (mm-mix (apply map f xss)))

(defcgen pan-lin [pos {:default 0}
                in {:default 0}]
  (:ar (* (cubed (clip:ar [(select (< pos -1/2) [(+ 7/8 (* -1 pos)) (+ 2 (*  2 pos))])
                           (select (> pos  1/2) [(+ 7/8 pos) (+ 2 (* -2 pos))])]
                          0 1)) in)))

(defcgen pan-exp [pos {:default 0}
                in {:default 0}]
  (:ar (let [x (abs pos)
             a (exp (* -14 x))
             b (exp (* -8 x))]
         (* in [(select (not-pos? pos) [a b])
                (select (not-pos? pos) [b a])]))))

(defcgen pan-gau [pos {:default 0}
                  in {:default 0}]
  (:ar (let [x (squared pos)
             a (exp (* -14 x))
             b (exp (* -8 x))]
         (* in [(select (not-pos? pos) [a b])
                (select (not-pos? pos) [b a])]))))

(defsound hole option-d
  (u/m-map (fn [phase i]
             (let [gate (lf-pulse 0.5 phase 1/4)
                   freq (-> (* dr (lin-exp (sin-osc 1/10 i) -1 1 100 10000))
                            (latch:ar (impulse:kr 32)))]
               (* gate 
                  (-> (rlpf (white-noise) freq 0.01)
                      (* (env-gen (env-perc 0.05 0.5) gate))
                      (free-verb 0.8 1)))))
         (u/n-range 0 1 10)
         (shuffle (u/n-range 0 1.5 10))))

(l4/control :hole :vol {:dur 64 :to 0})




(defsound flow option
  (mm-map #(let [freq (latch:ar (u/sin-r 0.3 110 4400) (impulse %))]
             (pan-gau (u/sin-r 0.06 -2 2) (sin-osc (* freq %2))))
          [0.05 0.07 0.19 0.09 0.09]
          [1 3/2 7/4 9/5 9/2]))


(defsound flow option
  (mm-map (fn [x y z] (let [pos (u/rg-lin (lf-tri y z) -2 2)]
                        (pan-gau pos (sin-osc (* 480 x)))))
          (range 1 8)
          (map #(/ 1 %) (range 3 11))
          (shuffle (u/n-range 0 2 8))))


(let [arr (repeatedly 8 #(ranged-rand 1000 8000))]
  (defsound karakara option
    (splay (repeatedly 8 #(let [gate (dust:kr 1)
                                freq (* (demand gate 0 (drand arr INF))
                                        (u/rg-lin (lf-pulse 8) 0.9 1))]
                            (* 5 (sin-osc freq)
                               (env-gen (env-perc 1e-6 3e-2) gate)))))))

(defsound strong-flow option
  (let [pos (sin-osc 0.05)
        car-freq (u/rg-exp (lf-noise0 (u/rg-exp (sin-osc 0.1) 0.001 16)) 50 800)
        freq (u/rg-exp (sin-osc 0.1) :-freq-min :-freq-max)]
    (-> (pan-gau pos (sin-osc freq (u/rg-lin (sin-osc car-freq) 0 (* 2 Math/PI))))
        (free-verb 0.5 0.1))))



(l4/control :strong-flow :-freq-min {:dur 8 :to 7600})
(l4/control :strong-flow :-freq-max {:dur 32 :to 9210})
