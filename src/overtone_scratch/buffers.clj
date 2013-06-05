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

(defn get-buffer-pos
  [m buf]
  (let
      [size (get (buffer-info buf) :size)
       curr-frame (* (now) (server-sample-rate))]
    (mod curr-frame size)))

(get (buffer-info mybuffer) :size)
(get-buffer-pos metro mybuffer)

(defn beat-player [m]
  (at (m) (kick))
  (apply-at (m (m)) #'beat-player [m])) ;; the original example also didn't do this at every beat!
(beat-player metro)

;; trying to write a one into the buffer at the current time
(buffer-write! mybuffer (get-buffer-pos metro mybuffer) [1])

(buffer-write! mybuffer 110250 [1])
(get (buffer-read mybuffer 110250 1) 0) ;; this seems to not work?!
(get (buffer-data mybuffer) 110250)

(stop)

;; trying to manyally copy the kick into mybuffer
(def kick-data (buffer-data kick))

(buffer-write! mybuffer (get-buffer-pos metro mybuffer) kick-data) ;; too much data
(buffer-write! mybuffer (get-buffer-pos metro mybuffer) (take 5000 kick-data)) ;; this sounds funny but may just be an artifact of having only the beginning of the sample
(buffer-write-relay! mybuffer (get-buffer-pos metro mybuffer) kick-data) ;; doesn't (always) seem to work. could it be a wraparound problem?!

;; next step would be to run a buf-rd that copies the kick onto mybuffer

(defcgen play-buf-once
  [buf {:default 0 :doc "Buffer to play"}]
  (:ar
   (let [idx (phasor:ar 0 1 0 (buf-frames:kr buf))]

     (* (env-gen:ar (lin-env 0 (buf-dur:kr buf) 0) :action FREE)
        (buf-rd 1 buf idx 0)))))

(defsynth copybuf [src-buf 0 dest-buf 0 live-bus 0 pos 0]
  (let [data     (play-buf-once src-buf)
        dest-pos (phasor:ar 0 1 pos (+ pos (buf-frames:kr dest-buf)))]

    (buf-wr [data] dest-buf dest-pos 0)
    (out live-bus data)
    ))

(show-graphviz-synth copybuf)

; copy kick to mybuffer (at the current position)
(copybuf :src-buf kick :dest-buf mybuffer :pos (get-buffer-pos metro mybuffer))

; empty the buffer
(buffer-write-relay! mybuffer (repeat (buffer-size mybuffer) 0))

;; loop-play the buffer
(defsynth buf-player [buf 0]
  ;; this used to be (buf-frames:ir 0) taken from some example. also, (buf-frames:ir buf) fails with some error message about mismatched ir/kr buffers?!
  (out 0 (buf-rd 1 buf (phasor:ar 0 1 0 (buf-frames:kr buf)))))

(buf-player (:id kick) 1)
(buf-player (:id mybuffer))
