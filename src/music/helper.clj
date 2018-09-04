(ns music.helper
  (:use [overtone.core]))

(defn lower-bound [in]
  (case (first in)
    latch:ar (lower-bound (second in))
    sin-osc -1
    sin-osc:kr -1
    lf-tri -1
    lf-tri:kr -1
    lf-saw -1
    lf-saw:kr -1
    white-noise -1
    lf-noise0 -1
    lf-noise1 -1
    lf-noise2 -1
    lf-noise0:kr -1
    lf-noise1:kr -1
    lf-noise2:kr -1
    lf-pulse 0
    lf-pulse:kr 0
    0))

(defmacro rg-lin [in lo hi]
  `(lin-lin ~in ~(lower-bound in) 1 ~lo ~hi))
(defmacro rg-exp [in lo hi]
  `(lin-exp ~in ~(lower-bound in) 1 ~lo ~hi))

(defmacro dq [trig arr] `(demand ~trig 0 (dseq ~arr INF)))
(defmacro dq:kr [trig arr] `(demand:kr ~trig 0 (dseq ~arr INF)))
(defmacro dt [dur arr]
  (if (coll? dur)
    `(duty:ar (dseq ~dur INF) 0 (dseq ~arr INF))
    `(duty:ar ~dur 0 (dseq ~arr INF))))

(defmacro dt:kr [dur arr]
  (if (coll? dur)
    `(duty:kr (dseq ~dur INF) 0 (dseq ~arr INF))
    `(duty:kr ~dur 0 (dseq ~arr INF))))

(defmacro eg-seq [levels dur trig]
  (let [durations (vec (repeat (dec (count levels)) dur))]
    `(env-gen (envelope ~levels ~durations) ~trig :action NO-ACTION)))

(defmacro eg-seq0 [levels dur trig]
  (let [levels (vec (interleave levels levels))]
    `(eg-seq ~levels ~dur ~trig)))

(defmacro mk-rythm [release durs]
  (let [size (count durs)
        levels (vec (flatten [(repeat size [0 1 1 0]) 0]))
        t (vec (mapcat (fn [dur] [0 release 0 (- dur release)]) durs))
        freq (/ 1 (sum durs))]
    `(env-gen (envelope ~levels ~t) (impulse ~freq))))

(defn primes [n]
  (letfn [(sieve [[xs ps]]
            (let [[p & more] xs]
              [(remove #(zero? (rem % p)) xs) (cons p ps)]))]
    (if (< n 2)
      []
      (->> [(range 2 (inc n)) nil]
           (iterate sieve)
           (drop-while #(< (ffirst %) (Math/sqrt n)))
           first
           (apply concat)
           (sort)))))
(defn slice [from to]
  (map vector (range from to) (drop 1 (range from to))))
(defn transpose [xss]
  (apply map list xss))
(defn step
  ([t] (step t 0))
  ([t dur]
   (env-gen (envelope [0 0 1] [t dur]))))

(defmacro switch [trig a b]
  `(let [t# ~trig]
     (~'+
      (~'* t# ~a)
      (~'* (~'- 1 t#) ~b))))

(defn vib
  ([vol freq wave] (lin-lin (wave freq (rand Math/PI))
                            -1 1
                            ((symbol "-") 1 vol) ((symbol "+") 1 vol)))
  ([vol freq] (vib vol freq sin-osc))
  ([vol] (vib vol 1 sin-osc)))


(defn m-map [f & xs]
  (let [result (apply map f xs)
        is-single (map? (first result))]
    (if is-single
      (mix result)
      (map mix result))))

(defmacro b-low-pass-coeff
  [freq rq] `(let [w0# (~(symbol "*") Math/PI 2 ~freq (/ 1 (server-sample-rate)))
                   cos_w0# (cos w0#)
                   i# (~(symbol "-") 1 cos_w0#)
                   alpha# (~(symbol "*") 0.5 ~rq (sin w0#))
                   b0rz# (~(symbol "/") 1 (~(symbol "+") 1 alpha#))
                   a0# (~(symbol "*") 0.5 i# b0rz#)
                   a1# (~(symbol "*") i# b0rz#)
                   b1# (~(symbol "*") cos_w0# 2 b0rz#)
                   b2# (~(symbol "*") -1 b0rz# (~(symbol "-") 1 alpha#))]
               [a0# a1# a0# b1# b2#]))

(defmacro b-low-pass-4 [in freq rq]
  `(let [coefs# (b-low-pass-coeff ~freq (sqrt ~rq))]
     (apply sos (apply sos ~in coefs#) coefs#)))

(defmacro pattern [trig ptn]
  `(~'* ~trig (demand ~trig 0 (dseq (flatten ~ptn) INF))))

(defmacro v0 [lower upper]
  `(lin-lin (lf-noise0 8) 0 1 ~lower ~upper))

(defmacro v0:kr [lower upper]
  `(lin-lin:kr (lf-noise0:kr 8) 0 1 ~lower ~upper))

(defmacro v1 [lower upper]
  `(lin-lin (lf-tri 8) 0 1 ~lower ~upper))

(defmacro v1:kr [lower upper]
  `(lin-lin:kr (lf-tri:kr 8) 0 1 ~lower ~upper))

(defn n-range [min max num]
  (range min max (/ (- max min) num)))

(defcgen vb-sin [freq {:default 440}
                 vol  {:default 0.1}
                 wide {:default 1.0}]
  (:ar (* (/ 2 (+ 1 vol))
          (mix (map #(* %2 (sin-osc:ar (+ %1 freq) (rand (* 2 Math/PI))))
                    [0 wide]
                    [1 vol]))))
  (:kr (* (/ 2 (+ 1 vol))
          (mix (map #(* %2 (sin-osc:kr (+ %1 freq) (rand (* 2 Math/PI))))
                    [0 wide]
                    [1 vol]))))
  (:default :ar))


(defn multiples [fst ratios]
  (reduce (fn [acc x] (conj acc (* x (last acc)))) [fst] ratios))

(defn poll-mouse-x [min max]
  (let [snd (mouse-x min max)]
    (poll (impulse 8) snd "mouse-x")
    snd))
(defn poll-mouse-y [min max]
  (let [snd (mouse-y min max)]
    (poll (impulse 8) snd "mouse-y")
    snd))
(defn poll-ex-mouse-x [min max]
  (let [snd (mouse-x min max EXP)]
    (poll (impulse 8) snd "mouse-x")
    snd))
(defn poll-ex-mouse-y [min max]
  (let [snd (mouse-y min max EXP)]
    (poll (impulse 8) snd "mouse-y")
    snd))

(defcgen once []
  (:kr (env-gen (envelope [0 1 1 0] [0 1e-6 0]))))

(defcgen sin-r [freq {:default 440}
                min-value {:default 0}
                max-value {:default 1}]
  (:ar (lin-lin (sin-osc:ar freq (* 2 Math/PI (rand)))
                -1 1 min-value max-value))
  (:kr (lin-lin (sin-osc:kr freq (* 2 Math/PI (rand)))
                -1 1 min-value max-value)))

(defn mm-mix [xss]
  (->> (transpose xss)
       (map mix)))

(defn mm-map [f & xss]
  (mm-mix (apply map f xss)))

(defmacro echo [source num min-delay max-delay decay ]
  `(reduce (fn [acc# x#] (allpass-c acc# (+ ~min-delay ~max-delay) x# ~decay))
          ~source
          (repeatedly ~num (fn [] (+ ~min-delay (rand ~max-delay))))))

(defn m->hz
  "Convert a midi note number to a frequency in hz."
  [note]
  (* 432.0 (java.lang.Math/pow 2.0 (/ (- note 69.0) 12.0))))

(defcgen m-hz [note {:default 0}]
  (:ar (* 432.0 (pow 2.0 (/ (- note 69.0) 12.0))))
  (:kr (* 432.0 (pow 2.0 (/ (- note 69.0) 12.0)))))

(defn detune [d f]
  (tanh (map f [(* -1 d) d])))

(defn detunr [d f]
  (tanh (map f [(- 1 d) (+ 1 d)])))

(defmacro eff [rate body]
  `(~'+ 1 (~'* ~rate (~'- ~body 1) )))

(defcgen smooth-step [keep-time {:default 1}
                      switch-time {:default 1}
                      min {:default 0}
                      max {:default 1}]
  (:ar (env-gen (envelope [0 min min max max min]
                          [0 keep-time switch-time keep-time switch-time]
                          :sine)
            (impulse (/ 1/2 (+ keep-time switch-time))))))

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

(defmacro reduce->
  [initial f & colls] (if (= (count colls) 1)
                        `(reduce ~f ~initial ~(first colls))
                        `(reduce ~f ~initial (music.helper/transpose ~(vec colls)))))

(defmacro switch->
  [in b eff] `(music.helper/switch ~b (-> ~in ~eff) ~in))

(defmacro throttle
  ([trig count] `(stepper ~trig 0 0 ~count))
  ([trig count phase] `(~'- (stepper ~trig 0 0 ~count) ~phase)))

(defn surround [n]
  (let [xs (range 1 n)]
    (concat (reverse xs) xs)))
