(ns overtone-scratch.tests
  (:use overtone.live))

(demo(saw [100 101]))
(overtone.inst.kick)

(definst mysin [freq 440]
  (* 0.5 (sin-osc freq)))

(mysin)

(ctl mysin :freq 500)

(kill mysin)

(definst quux [freq 440] (* 0.3 (saw freq)))

(quux)

(kill quux)

(definst trem [freq 440 depth 10 rate 6 length 3]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))

(trem)

(kill trem)

(trem 200 60 0.8)

(trem 60 30 0.2)

(def snare (sample (freesound-path 26903)))
(snare)

(use 'overtone.)

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

;(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(def metro (metronome 128))

(metro)

(defn player [beat]
  (at (metro beat) (kick))
  (at (metro (+ 0.5 beat)) (c-hat))
  ;(apply-at (metro (inc beat)) #'player (inc beat) [])
  )

(player (metro))

(definst o-hat [amp 0.8 t 0.5]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(defn swinger [beat]
  (at (metro beat) (o-hat))
  (at (metro (inc beat)) (c-hat))
  (at (metro (+ 1.65 beat)) (c-hat))
  ;(apply-at (metro (+ 2 beat)) #'swinger (+ 2 beat) [])
  )

(swinger (metro))

(demo 10 (lpf (saw 100) (mouse-x 40 5000 EXP)))

(demo 10 (hpf (saw 100) (mouse-x 40 5000 EXP)))

(demo 30 (bpf (saw 100) (mouse-x 40 5000 EXP) (mouse-y 0.01 1 LIN)))

;; here we generate a pulse of white noise, and pass it through a pluck filter
;; with a delay based on the given frequency
(let [freq 220]
  (demo (pluck (* (white-noise) (env-gen (perc 0.001 2) :action FREE)) 1 3 (/ 1 freq))))

(definst mypluck [freq 220]
  (pluck (* (white-noise) (env-gen (perc 0.001 2) :action FREE)) 2 5 (/ 1 freq))
  )

(mypluck 440)
(mypluck 220)

(kill mypluck)

(definst saw-wave [freq 440 attack 0.01 sustain 0.03 release 0.1 vol 0.8]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(saw-wave)

(saw-wave (midi->hz (note :C4))) ; This is C4! Surprised?

;; Let's make it even easier
(defn saw2 [music-note]
	(saw-wave (midi->hz (note music-note))))

;; Great!
(saw2 :A4)

;; this is one possible implementation of play-chord
(defn play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(play-chord [:A4 :C4 :D4])

;; We can play a chord progression on the synth
;; using times:
(defn chord-progression-time []
  (let [time (now)]
    (at time (play-chord (chord :C4 :major)))
    (at (+ 2000 time) (play-chord (chord :G3 :major)))
    (at (+ 3000 time) (play-chord (chord :F3 :sus4)))
    (at (+ 4300 time) (play-chord (chord :F3 :major)))
    (at (+ 5000 time) (play-chord (chord :G3 :major)))))

(chord-progression-time)

(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 14 beat-num)) (play-chord (chord :F3 :major)))
)

(def metro (metronome 150))

(chord-progression-beat metro (metro))

;; We can use recursion to keep playing the chord progression
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
  ;(apply-at (m (+ 16 beat-num)) chord-progression-beat m (+ 16 beat-num) [])
)

;; touch osc stuff

(def server (osc-server 44100 "osc-clj"))

(zero-conf-on)
(zero-conf-off)

;; debugging touch osc
(osc-listen server (fn [msg] (println msg)) :debug)
(osc-rm-listener server :debug)
(osc-handle server "/1/push" (fn [msg] (println msg)))

(osc-handle server "/7/push13" (fn [msg] (if (== 1 (first (:args msg))) (kick))))

;; buffers
(defn set-up-buffer [m beats]
  (buffer (* (server-sample-rate) (- (m (+ (m) beats)) (m)))))

;; talking to sam
(def ab (audio-bus))

(defsynth foo []
  (out ab (sin-osc 400)))


(defsynth bar []
  (out 0 (in ab)))

(def b (bar))

(def f (foo))

(stop)

;; midi

(connected-midi-devices)
(event-debug-on)
(event-debug-off)

(def kb (midi-in "Port 1"))
(def kbout (midi-out "Port 1"))

(midi-note-on kbout 60 100)
(midi-note-off kbout 60)

;; this is a Daft Punk's "Giorgio" theme playing on the midi instrument

(def repetition-sub-a (map note [:C5, :A3, :B4, :A3, :C5, :E5, :A3, :A4, :C5, :A3, :B4, :A3, :C5, :A4]))
(def repetition-a (concat (map note [:A4, :A3]) repetition-sub-a (map note [:A3, :A4]) repetition-sub-a))

(def metro (metronome (* 4 113)))
(metro)

(defn midi-player
  [beat notes multiplier]
  (let [n     (first notes)
        notes (next notes)
        next-beat (inc beat)]
    (when n
      (at (metro beat)
          (midi-note-on kbout n 100))
      (apply-at  (metro next-beat) #'midi-player [next-beat notes multiplier])
      )))

(midi-player (metro) repetition-a 1)
