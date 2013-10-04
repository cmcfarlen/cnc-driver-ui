(ns cncui.mill
  (:require [cncui.bin :as bin]
            [cncui.io :as sio])
  (:import [java.nio ByteBuffer ByteOrder]))

; message definitions
(defbinrecord PingMessage "b" [status])

; high level protocol
(defprotocol Mill
  (stop! [m])
  (move! [m [xdir xvel xacc xdist] [ydir yvel yacc ydist] [zdir zvel zacc zdist]])
  (move-x! [m [xdir xvel xacc xdist]])
  (move-y! [m [ydir yvel yacc ydist]])
  (move-z! [m [zdir zvel zacc zdist]]))


; message look like 0x1eaf <short size> <short type> <field...>

(defn write-message
  [port msg]
  (let [b (bin/pack msg)]
    (sio/write port b)))

(defn read-magic1
  [bstream]
  (loop [b (take 1 bstream)]
    (if (= b 0x1e)
      bstream
      (recur (take 1 bstream)))))

(defn read-magic
  [bstream]
  (loop [b (take 1 (read-magic1 bstream))]
    (if (= b 0xaf)
      bstream
      (recur (take 1 (read-magic1 bstream))))))

(defn read-size
  [bstream]
  (let [



