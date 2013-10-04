(ns cncui.io
  (:require [clojure.string :as string]
            [cncui.bin :as bin]
            [clojure.java.io :as io])
  (:import [gnu.io CommPortIdentifier CommPort SerialPort]
           [java.util UUID]
           [java.nio ByteBuffer]))

(defn port-names 
  "sequence of port names"
  []
  (map #(.getName %) (enumeration-seq (CommPortIdentifier/getPortIdentifiers))))

(defrecord Port [name ident port in out])

(defn open
  "Open a port by name"
  [name]
  (let [portid (CommPortIdentifier/getPortIdentifier name)
        n      (UUID/randomUUID)
        serial (.open portid (.toString n) 5000)]
    (Port. name portid serial (.getInputStream serial) (.getOutputStream serial))))

(defn open-pretend
  "Open a pretend port using files"
  [port-name in-file out-file]
  (Port. port-name 0 nil (io/input-stream (io/as-file in-file)) (io/output-stream (io/as-file out-file))))

(defn close
  "Close the port"
  [port]
  (let [in (:in port)
        out (:out port)
        ser (:port port)]
    (reduce (fn [_ s] (if-not (nil? s) (.close s))) 0 [in out ser])))

(defn write-stream 
  ([os i] (.write os i))
  ([os ba ofs cnt] (.write os ba ofs cnt)))

(defn read-stream 
  ([is] (.read is))
  ([is ba ofs cnt] (.read is ba ofs cnt)))

(defn write-byte [port v]
  (write-stream (:out port) (unchecked-byte v)))

(defn write-short [port v]
  (let [st (:out port)
        sv (unchecked-short v)]
    (write-stream st sv)
    (write-stream st (bit-shift-right sv 8))))

(defn write-int [port v]
  (let [st (:out port)
        iv (unchecked-int v)]
    (reduce (fn [_ bs] (write-stream st (bit-shift-right iv bs))) nil (range 0 32 8))))

(defn write-long [port v]
  (let [st (:out port)
        lv (unchecked-long v)]
    (reduce (fn [_ bs] (write-stream st (bit-shift-right lv bs))) nil (range 0 64 8))))

(defn read-byte [port]
  (unchecked-byte (read-stream (:in port))))

(defn read-short [port]
  (let [is (:in port)
        b1 (read-stream is)
        b2 (read-stream is)]
    (unchecked-short (bit-or (bit-shift-left b2 8) b1))))

(defn read-int [port]
  (let [is (:in port)
        b1 (read-stream is)
        b2 (read-stream is)
        b3 (read-stream is)
        b4 (read-stream is)]
    (unchecked-short 
      (bit-or 
        (bit-shift-left b4 24)
        (bit-shift-left b3 16)
        (bit-shift-left b2 8)
        b1))))

(defn read-long [port]
  (let [is (:in port)
        b1 (read-stream is)
        b2 (read-stream is)
        b3 (read-stream is)
        b4 (read-stream is)
        b5 (read-stream is)
        b6 (read-stream is)
        b7 (read-stream is)
        b8 (read-stream is)]
    (unchecked-short 
      (bit-or 
        (bit-shift-left b8 56)
        (bit-shift-left b7 48)
        (bit-shift-left b6 40)
        (bit-shift-left b5 32)
        (bit-shift-left b4 24)
        (bit-shift-left b3 16)
        (bit-shift-left b2 8)
        b1))))


(extend-protocol bin/BinStream
  Port
  (remaining? [port] true)
  (flip [port] port)
  (put-byte ([port v] (write-byte port v))
            ([port v ofs cnt] (write-stream (:out port) v ofs cnt)))
  (put-short [port v] (write-short port v))
  (put-int [port v] (write-int port v))
  (put-long [port v] (write-long port v))
  (get-byte ([port] (read-byte port))
            ([port ba ofs cnt] (read-stream (:in port) ba ofs cnt)))
  (get-short [port] (read-short port))
  (get-int [port] (read-int port))
  (get-long [port] (read-long port)))


(defn serial-seq
  [port]
  (lazy-seq (cons (read-byte port) (serial-seq port))))

(defn serial-reader
  [port]
  (clojure.java.io/reader (:in port)))


