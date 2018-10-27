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

(def option {:swap-option {:switch-dur 8
                           :fade-out-dur 32}
             :state {:initial (update-state {:count 0})
                     :update update-state}})
(def option-d (merge option {:synth-option {:type :detune}}))

(defsound music
  (merge option {:swap-option {:switch-dur (* 8 4)}
                 :period 64})
  (splay (let [n 18]
           (-> (map #(* (+ (sin-osc 0.04 %) (sin-osc 0.008 %))
                        (sin-osc (* (* (!! (if b2 400 1200)) (+ (sin-osc 0.01 %)
                                              (sin-osc (!! (if b0 0.03 0.1)) %)
                                              (* (sin-osc (!! (if b1 0.044 0.128)) %)
                                                 (sin-osc (!! (if b2 0.16 0.32)) %))))
                                    (sin-osc (* (!! (+ c1 (inc c3))) %2 100)))))
                    (range 1 n)
                    (u/n-range 0 3 n))
               (* n)))))


(l4/control :music :vol {:dur (* 8 32) :to 0})


(defsound typhoon
  {:swap-option {:switch-dur 8
                           :fade-out-dur 32}
             :state {:initial (update-state {:count 0})
                     :update update-state}}
  (* 32 (splay
         (tanh (* 640 (let [freq 1000]
                        (map #(reduce (fn [acc x] (sin-osc (* acc % freq x)))
                                      (sin-osc (* % 800))
                                      [(u/rg-exp (sin-osc 0.04) 0.1 1.2)
                                       0.43
                                       (u/rg-exp (sin-osc 0.13) 0.3 20.2)
                                       (u/rg-exp (sin-osc 0.16) 1 10.8)
                                       1.26])
                             (shuffle (u/n-range 1e-8 1 6)))))))))





(l4/stop :typhoon)




