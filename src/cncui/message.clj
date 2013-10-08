(ns cncui.message
  (:require [cncui.bin :as bin]
            [cncui.port :as port])
  (:import [java.nio ByteBuffer ByteOrder]))

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

(def message-agent (agent {:typemap {} :idmap {}}))

(defn register-type
  [a r-type r-id]
  (-> a
    (assoc-in [:typemap r-type] r-id)
    (assoc-in [:idmap r-id] r-type)))

(defn id->type
  [r-id]
  (-> @message-agent :idmap (get r-id)))

(defn type->id
  [r-type]
  (-> @message-agent :typemap (get r-type)))

;; There has to be a cleaner way for this
(defn empty-record
  "create an instance of binrecord with all null fields"
  [record-type]
  (let [cname (.getName record-type)
        lastdot (.lastIndexOf cname ".")
        nspace (.substring cname 0 lastdot)
        sname  (.substring cname (+ 1 lastdot))
        ctr (resolve (symbol (str nspace "/map->" sname)))]
    (ctr {})))

(defmacro defmessage
  [name type-id signature fields]
  `(do
     (bin/defbinrecord ~name ~signature ~fields)
     (send message-agent register-type ~name ~type-id)))

; message definitions
(defmessage PingMessage 0x00 "b" [status])
(defmessage ErrorMessage 0x01 "s" [status])
(defmessage InfoMessage 0x02 "s" [status])
(defmessage TestMessage 0x10 "is16" [counter message])


; high level protocol
(defprotocol Mill
  (stop! [m])
  (move! [m [xdir xvel xacc xdist] [ydir yvel yacc ydist] [zdir zvel zacc zdist]])
  (move-x! [m [xdir xvel xacc xdist]])
  (move-y! [m [ydir yvel yacc ydist]])
  (move-z! [m [zdir zvel zacc zdist]]))


; message look like 0x1eaf <short size> <short type> <field...>

(defn write-message
  [bstream msg]
  (let [sz (+ 6 (bin/binsize msg))
        id (type->id (type msg))]
    (-> bstream
        (bin/pack-short! 0x1eaf)
        (bin/pack-short! sz)
        (bin/pack-short! id)
        (bin/pack msg))))

(defn seek-byte
  [bstream v]
  (loop [b (bin/unpack-unsigned-byte! bstream)]
    (if (= b v)
      v
      (recur (bin/unpack-unsigned-byte! bstream)))))

(defn read-magic
  [bstream]
  (loop [m1 (seek-byte bstream 0xaf)
         m2 (bin/unpack-unsigned-byte! bstream)]
    (if (= m2 0x1e)
      0x1eaf
      (recur (seek-byte bstream 0xaf) (bin/unpack-unsigned-byte! bstream)))))

(defn read-message
  [bstream]
  (let [magic (read-magic bstream)
        sz    (bin/unpack-short! bstream)
        tp    (bin/unpack-short! bstream)
        mtype (id->type tp)]
    (bin/unpack (empty-record mtype) bstream)))

(defn message-seq
  [bstream]
  (lazy-seq (cons (read-message bstream) (message-seq bstream))))


