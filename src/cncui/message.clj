(ns cncui.message
  (:require [cncui.bin :as bin]
            [cncui.port :as port]
            [cncui.core :as core])
  (:import [java.nio ByteBuffer ByteOrder]))

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

(defprotocol MessageEndpoint
  (read-message [this msg])
  (write-message [this msg])
  (register-message [this msgtype msgid]))

(declare read-message)
(declare write-message)

(defn remove-if
  "remove and return k if it exists"
  [r k]
  (dosync
    (let [v (get @r k)]
      (if v
        (ref-set r (dissoc @r k)))
      v)))

(defn stop-reading
  [a]
  (assoc a :running false))

(defn assign-correlation
  "get the next correlation and associate it with a promise"
  [svc]
  (dosync
    (let [cntr (-> svc :state :corr-counter)
          disp (-> svc :state :promises)
          c (alter cntr inc)
          p (promise)]
      (alter disp assoc c p)
      [c p])))

(defn assoc-dispatch
  "associate a message type with a dispatch function"
  [svc tp df]
  (dosync
    (let [disp (-> svc :state :dispatch)]
      (alter disp assoc tp df))
    svc))

(defn send-command
  "Send a message and return a promise to get the response"
  [svc msg]
  (let [[c p] (assign-correlation svc)]
    (write-message svc msg c)
    p))

(defn read-next-message
  [a svc]
  (let [rval (read-message svc)
        msg (nth rval 0)
        tp (nth rval 1)
        corr (nth rval 2)]
    (when (:running a) (send *agent* read-next-message svc))
    (if-let [p (remove-if (-> svc :state :promises) corr)]
      (deliver p msg)
      (if-let [disp (remove-if (-> svc :state :dispatch) tp)]
        (disp svc msg)
        (print "unhandled message" msg)))
    (assoc a :count (inc (:count a)))))

(defn start-reading
  [a svc]
  (send *agent* read-next-message svc)
  (assoc a :running true))

(defn init-state
  [binstream]
  {
   :binstream binstream
   :corr-counter (ref 0N)
   :read-agent (agent { :count 0N :running false })
   :reg-ref (ref { :typemap {} :idmap {} })
   :promises (ref {})
   :dispatch (ref {})
   })

(defrecord MessageService [state]
  core/Service
  (start [svc]
    (do
      (when (-> state :binstream :in)
        (send (:read-agent state) start-reading svc))
      (core/notify "starting emssage service")
      svc))
  (stop [svc]
    (do
      (when (-> state :binstream :in)
        (send (:read-agent state) stop-reading))
      (core/notify "stopping message service" svc)
      svc)))

(defn message-service
  [binstream]
  (->MessageService (init-state binstream)))

(defn id->type
  [reg-ref r-id]
  (-> @reg-ref :idmap (get r-id)))

(defn type->id
  [reg-ref r-type]
  (-> @reg-ref :typemap (get r-type)))

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


(defn register-message
  [svc msg-type type-id]
  (let [reg (-> svc :state :reg-ref)]
    (dosync (ref-set reg (-> @reg
                             (assoc-in [:typemap msg-type] type-id)
                             (assoc-in [:idmap type-id] msg-type))))))

; message look like 0x1eaf <short size> <short type> <correlation> <field...>
(defn write-message
  ([svc msg]
   (write-message svc msg 0))
  ([svc msg corr]
   (let [bstream (-> svc :state :binstream)
         sz (+ 8 (bin/binsize msg))
         id (type->id (-> svc :state :reg-ref) (type msg))]
     (-> bstream
         (bin/put-short 0x1eaf)
         (bin/put-short sz)
         (bin/put-short id)
         (bin/put-short corr))
     (bin/pack msg bstream))))

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
  "return [msg typeid correlation]"
  [svc]
  (let [bstream (-> svc :state :binstream)
        magic (read-magic bstream)
        sz    (bin/get-short bstream)
        tp    (bin/get-short bstream)
        corr  (bin/get-short bstream)
        mtype (id->type (-> svc :state :reg-ref) tp)]
    [(bin/unpack (empty-record mtype) bstream) tp corr]))

(defn message-seq
  [svc]
  (lazy-seq (cons (first (read-message svc)) (message-seq svc))))


