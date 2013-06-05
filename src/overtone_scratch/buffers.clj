(ns overtone-scratch.buffers
  (:use overtone.live))

(def hat (sample (freesound-path 801)))
(def kick (sample (freesound-path 132584)))

; create a buffer of a length that is a multiple of metronome ticks
(defn create-synched-buffer [m num-ticks]
  (buffer (/ (* num-ticks (metro-tick m) (server-sample-rate)) 1000)))

(def metro (metronome 64))
(def mybuffer (create-synched-buffer metro 4))

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
(stop)

;; next steps:
;; - make arbitrary-length buffers work
;;   - when creating a buffer, start playing (at the start of a beat might make life easier?)
;;   - store the time when we start playing in a map together with a reference to the buffer
;;   - when adding to the buffer, get the time difference between now and when we started playing (mod length of buffer)
;;   - that index is where we want to start copying the sound to
;; - find out why many quick kicks produce an artifact in the buffer but not if multiple synths play?!
;; - make a variation of addnow that copies from a bus, so we can record out (be careful about node ordering!)
;; - give feedback through touchOSC?


; touch osc
(def server (osc-server 44100 "osc-clj"))

(zero-conf-on)
(zero-conf-off)

(osc-handle server "/7/push13" (fn [msg] (if (== 1 (first (:args msg))) (addnow kick))))
(osc-handle server "/7/push14" (fn [msg] (if (== 1 (first (:args msg))) (addnow hat))))

(osc-handle server "/7/push9" (fn [msg] (if (== 1 (first (:args msg))) (kick))))
(osc-handle server "/7/push10" (fn [msg] (if (== 1 (first (:args msg))) (hat))))

(osc-send server "/7/push13" 1)

(osc-rm-all-handlers)
