(ns cncui.port
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

(defn serial-seq
  [port]
  (lazy-seq (cons (bin/get-byte port) (serial-seq port))))

(defn serial-reader
  [port]
  (clojure.java.io/reader (:in port)))

