(ns llll-work.sin-osc-only
  (require [llll.core :as l4]
           [llll.clay.clay :as cl]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(connect-external-server "192.168.100.102" 57110)

(l4/control :music :vol {:dur 16 :to 1.0})
(connect-external-server "192.168.30.17" 57110)

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

(defsound music option
  (splay (let [n 20]
           (-> (map #(* (+ (sin-osc 0.4 %) (sin-osc 0.008 %))
                        (sin-osc (* (* 400 (* (+ (sin-osc 0.01 %)
                                                 (sin-osc 0.03 %)
                                                 (* (sin-osc 0.07 %)
                                                    (sin-osc 0.2 %)))))
                                    (sin-osc (* %2 100)))))
                    (range 1 n)
                    (u/n-range 0 3 n))
               (* n)))))

(l4/stop :music)



(l4/control :music :vol {:dur 256 :to 0.3})

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

(defn mm-mix [xss]
  (->> (u/transpose xss)
       (map mix)))

(defn mm-map [f & xss]
  (mm-mix (apply map f xss)))

(defsound flow option
  (let [freq (latch:ar (u/sin-r 0.3 440 2200) (impulse 1/7))]
    (pan-gau (u/sin-r 0.06 -2 2) (sin-osc freq))))

(l4/control :flow :vol {:dur 128 :to 1})

(defsound flow option
  (mm-map #(let [freq (latch:ar (u/sin-r 0.3 440 2200) (impulse %))]
             (pan-gau (u/sin-r % -2 2) (sin-osc (* freq %2))))
          [0.05 0.07 0.19]
          [1 3/2 7/4]))

(defsound flow option
  (mm-map #(let [freq (latch:ar (u/sin-r 0.3 440 4400) (impulse %))]
             (pan-gau (u/sin-r % -2 2) (sin-osc (* freq %2))))
          [0.05 0.07 0.19 0.09]
          [1 3/2 5/3 7/4 9/5]))

(defsound music {:swap-option {:switch-dur (* 8 32)}}
  (splay (let [n 24]
           (-> (map #(* (+ (sin-osc 0.4 %) (sin-osc 0.008 %))
                        (sin-osc (* (* 400 (* (+ (sin-osc 0.1 %)
                                                 (sin-osc 0.13 %)
                                                 (* (sin-osc 0.7 %)
                                                    (sin-osc 1.2 %)))))
                                    (sin-osc (* %2 100)))))
                    (range 1 n)
                    (u/n-range 0 3 n))
               (* n)))))

(l4/control :music :vol {:dur 256 :to 0})

(defsound keep
  {:swap-option {:switch-dur (* 8 16)}}
  (* 5 (splay (map #(* %1 (sin-osc %2) (sin-osc (* %3 600)))
                    (shuffle (range 0 1 0.1))
                    (shuffle (range 0.01 0.05 0.002))
                    (shuffle (range 1 8))))))

(l4/control :keep :vol {:dur 256 :to 0.2})

(defsound keep
  {:swap-option {:switch-dur 128}}
  (* 5 (splay (map #(* %1 (sin-osc %2) (sin-osc (* %3 600 (ranged-rand 0.9 1.1))))
                    (shuffle (range 0 1 0.1))
                    (shuffle (range 0.01 0.05 0.002))
                    (shuffle (range 1 8))))))

(defsound dark option
  (let [eff (+ 1 (sin-osc 0.002))]
    (* 20 (splay (mapcat (fn [x]
                           (map #(* (sin-osc 0.1 (* x %2 Math/PI)) (sin-osc (* (- x eff)  %)))
                                (range 150 1200 234)
                                (shuffle (range 0 2 0.2))))
                         (take 10 (iterate #(* 1.23 %) 1)))))))


(defsound radio option-d
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

(defsound spin option-d
  (reduce (fn [acc x] (sin-osc (*  x acc))) (sin-osc (* 100))
          [2000 8000 500 ]))

(defsound typhoon option
  (* 32 (splay
         (tanh (* 640 (let [freq 1000]
                        (map #(reduce (fn [acc x] (sin-osc (* acc % freq x)))
                                      (sin-osc (* % 800))
                                      [3.2
                                       1.23
                                       0.43
                                       (u/rg-exp (sin-osc 0.734) 1 1.8)
                                       (u/rg-exp (sin-osc 0.834) 1 20.3)
                                       (u/rg-exp (sin-osc 0.134) 1 18.2)
                                       ])
                             (shuffle (u/n-range 1e-8 1 6)))))))))







(defsound talk option-d
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
    (* 1/2 (sin-osc (* base-freq (* (sin-osc 0.07) (sin-osc 80))))
       (sin-osc 0.8))))


(defsound typhoon option
  (* 32 (splay
         (tanh (* 640 (let [freq 1000]
                        (map #(reduce (fn [acc x] (sin-osc (* acc % freq x)))
                                      (sin-osc (* % 800))
                                      [3.2 0.23 0.43
                                       (u/rg-exp (sin-osc 0.834) 1 1.2)
                                       (u/rg-exp (sin-osc 0.734) 1 2.2)
                                       (u/rg-exp (sin-osc 0.134) 1 8.2)
                                       ])
                             (shuffle (u/n-range 1e-8 1 6)))))))))

(l4/control :typhoon :vol {:dur 256 :to 0})

(l4/control :typhoon :vol {:dur 16 :to 0.2})

(defsound alart option-d
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
              (u/n-range 1e-5 1 4))))

(defsound with-saw option-d
  (let [freq 1000]
    (* (reduce (fn [acc x] (+ acc (sin-osc (* x freq dr))))
               (take 8 (reductions * (cycle [1 1.3 1.2 0.9 0.8]))))
       (+ (reduce * (repeatedly 30 #(sin-osc (* 3.8 (sin-osc 0.02)))))
          (reduce * (repeatedly 30 #(sin-osc (* 1.8 (sin-osc 0.4)))))))))







