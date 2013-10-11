(ns cncui.message
  (:require [cncui.bin :as bin]
            [cncui.port :as port]
            [cncui.core :as core])
  (:import [java.nio ByteBuffer ByteOrder]))

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

(declare read-message)

(defn stop-reading
  [a]
  (assoc a :running false))

(defn read-next-message
  [a svc]
  (let [msg (read-message svc)]
    (when (:running a) (send *agent* read-next-message svc))
    (print msg)
    (assoc a :count (inc (:count a)))))

(defn start-reading
  [a svc]
  (send *agent* read-next-message svc)
  (assoc a :running true))


(defrecord MessageService [reg-agent binstream read-agent]
  core/Service
  (start [svc]
    (do
      (when (:in binstream)
        (send read-agent start-reading svc))
      (core/notify "starting emssage service")
      svc))
  (stop [svc]
    (do
      (when (:in binstream)
        (send read-agent stop-reading))
      (core/notify "stopping message service" svc)
      svc)))

(defn id->type
  [message-agent r-id]
  (-> @message-agent :idmap (get r-id)))

(defn type->id
  [message-agent r-type]
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

(defn message-service
  [binstream]
  (->MessageService (ref { :typemap {} :idmap {} }) binstream (agent {:count 0 :running false})))

(defn register-message
  [svc msg-type type-id]
  (let [reg (:reg-agent svc)]
    (dosync (ref-set reg (-> @reg
                             (assoc-in [:typemap msg-type] type-id)
                             (assoc-in [:idmap type-id] msg-type))))))

; message look like 0x1eaf <short size> <short type> <correlation> <field...>
(defn write-message
  [svc msg]
  (let [bstream (:binstream svc)
        sz (+ 8 (bin/binsize msg))
        id (type->id (:reg-agent svc) (type msg))]
    (-> bstream
        (bin/put-short 0x1eaf)
        (bin/put-short sz)
        (bin/put-short id)
        (bin/put-short 0))
    (bin/pack msg bstream)))

(defn seek-byte
  [bstream v]
  (loop [b (bin/get-unsigned-byte bstream)]
    (if (= b v)
      v
      (recur (bin/get-unsigned-byte bstream)))))

(defn read-magic
  [bstream]
  (loop [m1 (seek-byte bstream 0xaf)
         m2 (bin/get-unsigned-byte bstream)]
    (if (= m2 0x1e)
      0x1eaf
      (recur (seek-byte bstream 0xaf) (bin/get-unsigned-byte bstream)))))

(defn read-message
  [svc]
  (let [bstream (:binstream svc)
        magic (read-magic bstream)
        sz    (bin/get-short bstream)
        tp    (bin/get-short bstream)
        corr  (bin/get-short bstream)
        mtype (id->type (:reg-agent svc) tp)]
    (bin/unpack (empty-record mtype) bstream)))

(defn message-seq
  [svc]
  (lazy-seq (cons (read-message svc) (message-seq svc))))


