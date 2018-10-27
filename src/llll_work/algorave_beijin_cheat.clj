(ns llll-work.algorave-beijin
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

(l4/initialize {:osc-client false
                :osc-path "/hello"})

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
  (splay (let [n 4]
           (-> (map #(* (sin-osc 0.008 %)
                        (sin-osc (* (* 400 (sin-osc 0.01 %))
                                    (sin-osc (* %2 100)))))
                    (range 1 n)
                    (u/n-range 0 3 n))
               (* n)))))

(defsound music option
  (splay (let [n 4]
           (-> (map #(* (+ (sin-osc 0.4 %) (sin-osc 0.008 %))
                        (sin-osc (* (* 400 (sin-osc 0.01 %))
                                    (sin-osc (* %2 100)))))
                    (range 1 n)
                    (u/n-range 0 3 n))
               (* n)))))

(defsound music option
  (splay (let [n 4]
           (-> (map #(* (+ (sin-osc 0.4 %) (sin-osc 0.008 %))
                        (sin-osc (* (* 400 (* (+ (sin-osc 0.01 %)
                                                 (sin-osc 0.03 %)
                                                 (* (sin-osc 0.56 %)
                                                    (sin-osc 0.2 %)))))
                                    (sin-osc (* %2 100)))))
                    (range 1 n)
                    (u/n-range 0 3 n))
               (* n)))))

(defsound music option
  (splay (let [n 24]
           (-> (map #(* (+ (sin-osc 0.4 %) (sin-osc (!! (if b0 0.008 0.1)) %))
                        (sin-osc (* (* 400 (* (+ (sin-osc (!! (if b1 0.01 2.3)) %)
                                                 (sin-osc 0.13 %)
                                                 (* (sin-osc (!! (if b2 0.07 1.8)) %)
                                                    (sin-osc 1.2 %)))))
                                    (sin-osc (* %2 100 (!! (+ c3 (inc c1))))))))
                    (range 1 n)
                    (u/n-range 0 3 n))
               (* n)))))

(defsound keep
  {:swap-option {:switch-dur (* 8 16)}}
  (* 5 (splay (map #(* %1 (sin-osc %2) (sin-osc (* %3 600)))
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

(defsound typhoon option
  (* 32 (splay
         (tanh (* 640 (let [freq 1000]
                        (map #(reduce (fn [acc x] (sin-osc (* acc % freq x)))
                                      (sin-osc (* % 800))
                                      [3.2
                                       0.43
                                       3.23
                                       (u/rg-exp (sin-osc 0.734) 1 1.8)
                                       ;; (u/rg-exp (sin-osc 0.834) 1 20.3)
                                       (u/rg-exp (sin-osc 0.134) 1 18.2)
                                       ])
                             (shuffle (u/n-range 1e-8 1 6)))))))))

(defsound typhoon
  {:swap-option {:switch-dur 8}}
  (* 32 (splay
         (tanh (* 640 (let [freq 1000]
                        (map #(reduce (fn [acc x] (sin-osc (* acc % freq x)))
                                      (sin-osc (* % 800))
                                      [1.23
                                       2.2
                                       ;; 0.43
                                       (u/rg-exp (sin-osc 0.02) 1 1.4)
                                       ;; (u/rg-exp (sin-osc 0.734) 1 2.2)
                                       ;; (u/rg-exp (sin-osc 0.134) 1 8.2)
                                       ])
                             (shuffle (u/n-range 1e-8 1 6)))))))))
