(ns llll-work.core
  (require [llll.core :as l4]
           [llll.macro.defsound :refer :all]
           [llll.macro.defpattern :refer :all]
           [llll.macro.control :refer :all])
  ;; (use [overtone.live])
  (use [overtone.core])
  ;; eval below code to connect sc server
  ;; (connect-external-server "localhost" 57110) 
  )

(connect-external-server "localhost" 57110)
(kill-server)

(l4/initialize {})
(l4/finish)


(control :first-sound :vol {:set 1})
(control :first-sound :-freq {:set 800})

(defsound test
  {}
  (let [gate-freq (lin-lin (sin-osc:kr 0.01) -1 1 1/4 2)]
    (splay (->> (cycle (concat (chord :C4 :minor)
                               (chord :E5 :minor)))
                (map midi->hz)
                (map #(* 16 (pulse %2 (-> (sin-osc 3.3)
                                          (lin-lin -1 1
                                                   0.1 0.9)))
                         (env-gen (env-perc 0.05 0.5) (impulse gate-freq %1)))
                     (range 0 1 1/12))))))

(l4/control :test :vol {:dur 16 :to 1})


(defsound first-sound
  {:synth-option {:type :detune :amount 0.08}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 4}
  (* (hpf (white-noise) 2000)
     (env-gen (env-perc 0 0.5) (impulse 1))
     ))





(l4/control :first-sound :-freq {:dur 100 :to 1000})
(l4/control :first-sound :-freq {:set 400})
(l4/control :first-sound :-freq {:set 16000})

(l4/control :defsynth-l4 :vol {:dur 16 :to 0.1})


(l4/control :test :vol {:dur 16 :to 1})


(l4/control :first-sound :-freq {:dur 1 :to 800})

(do (control :first-sound :-freq {:dur 32 :to 300})
    (control :first-sound :vol {:dur 32 :to 1}))

(l4/stop :first-sound)

(defsynth-l4 ping [freq 100 long 1]
  (* (pulse freq)
     (env-gen (env-perc 0.01 (* long dur)) :action FREE)))

(defsynth-l4 ping-2 [freq 100]
  (* (saw freq)
     (env-gen (envelope [0 1 1 0] [0.01 dur 0.03]) :action FREE)))

(defpattern first-pattern
  {:table {:p (synth ping)
           :p2 (synth ping-2)}
   :state {:initial {:hoge "hello"}
           :update identity}
   :period 32}
  (->> (| :p * 16)
       (=| :freq [800 100 ])
       (=| {:cycle? false} :freq [2800 1600])
       (=| :long [1/4 1/8 1/8 1] )))

(l4/control :first-pattern :vol {:dur 16 :to 1})

(defpattern hoge
  {:state {:initial {:ch :C4}}
   :table {:p (synth ping)
           :p2 (synth ping-2)}}
  (&| (map (fn [freq c] (->> (| :p * c :p2)
                             (=| :freq freq)))
           (->> (chord ch :minor)
                (map midi->hz))
           [1 3 8])))

(defsynth-l4 drum [freq 1000]
  (* 128
     (bpf (white-noise) freq 0.1)
     (env-gen (env-perc 0.05 dur) :action FREE)))


(defpattern first-pattern
  {:table {:a (control :first-sound :-freq)
           :b (control :second-sound :-freq)
           :d (synth drum)}
   :state {:initial {:count 1}
           :update #(update % :count inc)}
   :period 32}
  (&|
   (->> (| :d * (inc (mod count 8)))
        (=| {:cycle? (= (mod count 2) 0)} :freq (if (= (mod count 4) 0)
                                                  [1000 800]
                                                  (range 400 1000 100))))
   (--> (| :a*8) 1760 > [3] 880 [2] >| 300)))






(l4/stop :first-pattern)
(l4/finish)


(defsynth-l4 ping [note 0]
  {:type :detune}
  (* (sin-osc (* dr (midicps note)))
     (env-gen (env-perc (* 1/16 dur) dur) :action FREE)))

(defsynth-l4 super-saw [note 0]
  (* (splay (repeatedly 8 #(saw (* (lin-lin (sin-osc 0.8) -1 1 0.999 1.001) (midicps note)))))
     (env-gen (env-perc (* 1/4 dur) dur) :action FREE)))


(defpattern test
  {:table {:a (synth super-saw)}
   :state {:initial {:ch (map (partial apply chord) (cycle [[:C6 :minor] [:F6 :major] [:Db6 :major] [:Eb6 :major]]))
                     :n (cycle (concat [4 2 8 8] [4 2 8 8] [6 5 4 1]))
                     :m (cycle [1 1 1 2 4])}
           :update #(-> % (update :ch rest) (update :n rest) (update :m rest))}
   :period 32}
  (&| (map (fn [y] (+| (->> (| :a * (* y (first n)))
                            (=| :note (mapcat (fn [p] (map #(+ % (* 12 p)) (first ch)))
                                              (range 0 (first m)))))
                       (->> (| :a * (* (second n) (second m)))
                            (=| :note (second ch)))))
           (range 1 3))))

(demo (let [freq 440]

        (sin-osc freq)))

(defsound test
  {}
  (let [a (distort (comb-n (bpf (+ (* 7.5 (local-in 2))
                                   (* 0.2 (saw [32 33])))
                                (* (pow 2 (* 4 (lf-noise0:kr 4/3)))
                                   300)
                                0.1)))]
    (local-out a)
    a))


(defsound test
  {}
  (* (splay (ringz (impulse [2 1 4] [0.1 0.11 0.12])
                   [0.1 0.1 0.5]))
     (env-gen:kr (envelope [1 1 0] [120 10]) :action FREE)))

(defsound test
  {}
  (sum (map (fn [k] (reduce (fn [acc i] (* acc (* (sin-osc (* i k k) (sin-osc (/ (pow (* i k) i) [4 5])))
                                                  (decay (dust:kr (pow 1/4 i)) (+ (sin-osc 0.1) k i))
                                                  (* 999 k))))
                            1 (range 1 9)))
            (range 16))))

(l4/stop :test)






(require 'yasnippet)


(defsound test
  {}
  (sin-osc (lin-lin (lf-noise0 8) -1 1 300 1000)))

(l4/stop :test)

