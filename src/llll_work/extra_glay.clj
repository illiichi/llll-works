(ns llll-work.extra-glay
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll-work.util :as u])
  (use [overtone.core]))

(connect-external-server "localhost" 57110)
(kill-server)

(recording-start (str "/home/maeda/code/clojure/llll-works/record/"
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


(for [x (range 1 count)]
  (->> (| :a*x)
       (=| :freq (map (fn [y] (ranged-freq (/ (+ x y) 16)))))))

(defsound two-tone
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  [(section {:dur 7}
            (let [freq-base (u/rg-exp (sin-osc (!! (+ 3.3 c0))) 100 2000)
                  snd (u/m-map #(let [gate (lf-pulse % 1/2 (!! (if b1 1/4 3/4)))
                                      freq (latch:ar freq-base gate)]
                                  (* (sin-osc (* dr %2 freq)) gate))
                               [:f6 :f8 :f4 :f3]
                               [10 10 38 6])
                  freq (u/rg-exp (u/m-map lf-saw [1.2
                                                  (!! (if b1 0.8 2.7))
                                                  (!! (if b0 0.7 3.7))]) 200 2000)]
              (-> snd
                  (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [1 3 1/4 1/2 4])
                  (* 8) distort)))
   (section (let [freq (u/rg-exp (u/m-map lf-saw [2.3 3.8 2.8])
                                 (!! (if b1 100 800))
                                 (!! (if b0 4880 2800)))
                  snd (mix (sin-osc (* dr [1200 3200 640])))]
              (-> snd
                  (u/switch-> (!! (= c0 3))
                              (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [1 3 1/2 4]))
                  (u/switch-> (line 1 0 dur)
                              (u/reduce-> (fn [acc x] (ringz acc x dur)) [(* 2 freq) freq]))
                  (free-verb 1 10)
                  (* 8) distort)))])


(defsound two-tone
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 32}
  (let [freq (lin-lin (lf-pulse (u/dt:kr :t8 [:f4 :f2 :f8])) 0 1 800 950)
        base-freq (u/rg-lin (u/m-map lf-pulse [1/10 0.34 0.25 2.5])
                            (* 4 freq) (* 10 freq dr))
        snd (mix (sin-osc (* dr base-freq [1 3/2 (!! (if b0 7/3 5/2))])))]
    (-> snd
        (u/switch-> (!! (= c0 3))
                    (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [3/2 2]))
        (u/reduce-> (fn [acc x] (ringz acc x (!! (if b3 0.01 0.08))))
                    [freq base-freq (* 3/2 base-freq)])
        (free-verb 0.4 1)
        (* 4) distort)))

(defsound two-tone
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  [(section
    {:dur 1}
    (-> (u/m-map #(let [gate (lf-pulse (* :f2 %) 1/2 (!! (if b0 1/12 3/4)))
                        freq (latch:ar (u/rg-exp (sin-osc 0.3) 100 1000) gate)]
                    (-> (sin-osc (* dr freq %2))
                        (u/reduce-> (fn [acc x] (sin-osc (* freq acc x))) [1 1/4 1/2])
                        (* gate)))
                 [1/3 1/4 1/7 2/3]
                 [10 30 32 15])
        (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000))
        (* 8)
        distort))
   (section
    {:dur 1}
    (-> (let [f-env (env-gen (envelope [(!! (if b0 2 1/2))
                                        (!! (if b1 2 1/2))
                                        (!! (if b2 2 1/2))]
                                       [(* 1/4 dur) (* 3/4 dur)]))
              freq 1200]
          (u/m-map #(u/switch (line 1 0 dur)
                              (rlpf (white-noise) (* f-env dr %) 0.01)
                              (sin-osc (* f-env dr %)))
                   [freq (* freq 3/2) (* freq (!! (if b0 4 1/4)))]))
        (u/reduce-> (fn [acc x] (comb-l acc x x 0.2)) [0.01 0.03 0.008])
        (u/switch-> (!! b2) (ringz (u/rg-exp (u/m-map lf-pulse [0.3 0.2 0.5]) 100 10000)))
        (u/switch-> (!! b1) (moog-ff (u/rg-exp (u/m-map lf-pulse [1.3 0.2 0.5]) 1000 3000)))
        (* 8)
        distort))])



(defsound two-tone
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  [(section {:dur 1}
            (let [snd (u/m-map #(let [gate (lf-pulse %2 1/2 (!! (if b0 1/8 3/4)))
                                    freq (latch:ar (u/rg-exp (sin-osc 3.3)
                                                           100
                                                           (!! (if b0 1000 2000))) gate)]
                                (* (sin-osc (* dr % freq))
                                   gate))
                             [10 15 46]
                             [:f6 :f8 :f4])]
              (-> snd
                  (u/reduce-> (fn [acc [x y]] (-> acc (comb-l  x x 0.3)
                                                  (ringz  y 0.02))) [0.01 0.02] [(!! (* c0 1000))
                                                                                 (!! (if b0 4000 3000))])
                  (* 8) distort)))
   (section (let [snd (rlpf (white-noise) (* dr [1200 1800]) 0.0001)
                  freq (u/rg-exp (u/m-map lf-pulse [(!! (if b0 10.3 2.8))
                                                (!! (if b1 0.8 0.6))]) 80 1800)]
              (-> snd
                  (u/reduce-> (fn [acc x] (sin-osc (* acc x freq))) [1/2 1 2])
                  (free-verb (!! (if b1 1 0.3)) (!! (if b2 1 0.5)))
                  (* 8) distort)))])

(defsound two-tone
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  [(section {:dur 1}
            (let [snd (u/m-map #(let [gate (lf-pulse %2 1/2 1/4)
                                    freq (latch:ar (u/rg-exp (sin-osc (!! (if b0 8.3 0.3)))
                                                           100 2000) gate)]
                                (* (sin-osc (* dr % freq)) gate))
                             [5 18 28 4 8]
                             [:f6 :f8 :f4 :f7 :f5])]
              (-> snd
                  (u/reduce-> (fn [acc [x y]] (ringz (comb-l acc x x 0.3)
                                                     y 0.01))
                              [0.01 0.02]
                              [(!! (if b2 1000 500)) (!! (if b0 800 3200))])
                  (* 8) distort)))
   (section (let [f-env (env-gen (envelope [0 6 1] [(!! (if b0 0.05 0.4)) (!! (if b2 0.1 0.5))]))
                  freq (* f-env (u/rg-exp (lf-saw (!! (if b2 -4 16))) 800 (!! (if b0 1600 520))))
                  snd (u/switch (lf-pulse 8 0 1/4) (sin-osc (* dr freq))
                              (bpf (gray-noise) (* dr freq)))]
              (-> snd
                  (u/switch-> (line 1 0 dur)
                              (u/reduce-> (fn [acc x] (sin-osc (* acc x freq)))
                                          [1 1/4 (!! (if b0 1/2 1/4)) 3]))
                  (u/reduce-> (fn [acc x] (comb-l acc x x 0.3)) [0.01 0.02])
                  (* 8) distort)))])


(defsound two-tone
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  [(section {:dur 7}
            (let [snd (u/m-map #(let [gate (lf-pulse %1 1/2 (!! (if b3 3/4 1/8)))]
                                (* (sin-osc (* dr %2)) gate))
                             [:f6 :f8 :f5 :f7]
                             [(!! (if b2 1000 1200)) 2000 (!! (if b0 4200 3000)) 700])
                  freq (u/dq:kr (impulse :f4) [(!! (if b2 1800 800)) 4200 (!! (* c0 800)) 1200])]
              (-> snd
                  (u/reduce-> (fn [acc x] (comb-l acc x x 0.3)) [0.01 0.008])
                  (u/reduce-> (fn [acc x] (sin-osc (* acc x))) [300 (* 1/4 freq) freq])
                  (* 8) distort)))
   (section
    {:dur 1}
    (let [snd (u/m-map #(sin-osc (* dr (u/rg-lin (lf-pulse % (rand)) 200 %2)))
                     [8 12 4 6]
                     [1800 5600 400 1600])
          freq (u/rg-exp (u/m-map sin-osc [20.2 (!! (if b3 2.02 330)) 40.08]) (!! (if b1 400 1000)) 8000)]
      (-> snd
          (u/reduce-> (fn [acc [x delay]] (ringz acc x delay))
                      [800 1200 1600]
                      [0.08 0.12 0.016])
          (u/switch-> (line 0 1 dur)
                      (u/reduce-> (fn [acc x] (comb-l acc x x 0.3)) [0.01 0.02]))
          (u/reduce-> (fn [acc x] (sin-osc (* acc x))) [300 (* 1/4 freq) freq])
          (* 8) distort)))])


(defsound two-tone
  {:state {:initial (update-state {:count 0})
           :update update-state}
   :synth-option {:type :detune}
   :period 64}
  [(section
    {:dur 1}
    (let [snd (mix (map #(let [gate (lf-pulse % 1/2
                                              (!! (if b2 1/12 3/4)))]
                           (* (sin-osc (* dr %2)) gate))
                        [:f6 :f8 :f7 :f4]
                        [1000 1500 800 8000]))]
      (-> snd
          (u/switch->
           (!! b0)
           (u/reduce-> (fn [acc x] (sin-osc (* acc x)))
                       [140 680 1600 1400]))
          (* 8) distort)))
   (section
    {:dur 1}
    (-> (u/m-map #(let [freq (u/rg-exp (lf-saw %)
                                   (* %2 (!! (if b0 1200 120)))
                                   (* %2 (!! (if b1 (* c0 200) 4800))))
                      snd (sin-osc (* dr freq))]
                  snd)
               [:f8 :f3 :f1 :f7]
               [1 3/2 1/4 4])
        (u/switch-> (line 0 1 dur)
                    (u/reduce-> (fn [acc [x delay]] (comb-l acc x x delay))
                                [0.01 0.005 0.008]
                                [0.2 0.5 0.01]))
        (* 8)
        distort))])

