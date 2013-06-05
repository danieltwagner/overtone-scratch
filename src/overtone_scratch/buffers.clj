(ns overtone-scratch.buffers
  (:use overtone.live))

(def hat (sample (freesound-path 801)))
(hat)

(def kick (sample (freesound-path 132584)))
(kick)

; create a buffer of a length that is a multiple of metronome ticks
(defn set-up-buffer [m num-ticks]
  (buffer (/ (* num-ticks (metro-tick m) (server-sample-rate)) 1000)))


(def metro (metronome 64))
(def mybuffer (set-up-buffer metro 4))

(defn get-buffer-pos [buf]
  (let
      [size (buffer-size buf)
       curr-frame (* (now) (server-sample-rate) 0.001)]
    (mod curr-frame size)))

(buffer-size mybuffer)
(get-buffer-pos mybuffer)

(defn beat-player [m]
  (at (m) (kick))
  (apply-at (m (m)) #'beat-player [m])) ;; the original example also didn't do this at every beat!
(beat-player metro)

;; trying to write a one into the buffer at the current time
(buffer-write! mybuffer (get-buffer-pos mybuffer) [1])

(buffer-write! mybuffer 110250 [1])
(get (buffer-read mybuffer 110250 1) 0) ;; this seems to not work?!
(get (buffer-data mybuffer) 110250)

(stop)

;; next step would be to run a buf-rd that copies the kick onto mybuffer

(defcgen play-buf-once
  [buf {:default 0 :doc "Buffer to play"}]
  (:ar
   (let [idx (phasor:ar 0 1 0 (buf-frames:kr buf))]

     (* (env-gen:ar (lin-env 0 (buf-dur:kr buf) 0) :action FREE)
        (buf-rd 1 buf idx 0)))))

(defsynth copybuf [src-buf 0 dest-buf 0 live-bus 0 pos 0]
  (let [data     (play-buf-once src-buf)
        dest-pos (phasor:ar 0 1 pos (+ pos (buf-frames:kr dest-buf)))
        dest-val (buf-rd 1 dest-buf dest-pos 0)]

    (buf-wr [(+ data dest-val)] dest-buf dest-pos 0)
    (out live-bus data)
    ))

(show-graphviz-synth copybuf)

;; loop-play the buffer
(defsynth buf-player [buf 0]
  ;; this used to be (buf-frames:ir 0) taken from some example. also, (buf-frames:ir buf) fails with some error message about mismatched ir/kr buffers?!
  (out 0 (buf-rd 1 buf (phasor:ar 0 1 0 (buf-frames:kr buf)))))

(buf-player (:id kick) 1)

; synchronize playback of our buffer to the the metronome

(defn metro-buf-player [m buf]
  (at (metro-bar m (metro-bar m)) (buf-player (:id buf))))

(metro-buf-player metro mybuffer)

(defn time-passed-in-bar [m]
  (let [lastbar (metro-bar m (dec (metro-bar m)))]
    (- (now) lastbar)))

(time-passed-in-bar metro)

(defn addnow [buf] (copybuf :src-buf buf :dest-buf mybuffer :pos (* (time-passed-in-bar metro) (server-sample-rate) 0.001)))
(addnow kick)

; empty the buffer
(buffer-write-relay! mybuffer (repeat (buffer-size mybuffer) 0))





; touch osc
(def server (osc-server 44100 "osc-clj"))

(zero-conf-on)
(zero-conf-off)

(osc-handle server "/7/push13" (fn [msg] (if (== 1 (first (:args msg))) (addnow kick))))
(osc-handle server "/7/push14" (fn [msg] (if (== 1 (first (:args msg))) (addnow hat))))

(osc-handle server "/7/push9" (fn [msg] (if (== 1 (first (:args msg))) (kick))))
(osc-handle server "/7/push10" (fn [msg] (if (== 1 (first (:args msg))) (hat))))

(osc-rm-all-handlers)
