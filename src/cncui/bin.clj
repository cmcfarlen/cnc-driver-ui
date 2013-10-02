(ns cncui.bin
  (:use [clojure.java.io])
  (:import [java.nio ByteBuffer]))


(defn pack-byte [buf v]
  (.put buf (unchecked-byte (bit-and 0xff v)))
  buf)

(defn pack-short [buf v]
  (.putShort buf (unchecked-short (bit-and 0xffff v))))

(defn pack-int [buf v]
  (.putInt buf (unchecked-int (bit-and 0xffffffff v))))

(defn pack-str [buf s w]
  (let [l (.length s)]
    (if (< l w)
      (-> buf
          (.put (.getBytes s))
          (.put (byte-array (- w l))))
      (.put buf (.getBytes s) 0 w))))

(defn unpack-byte [buf]
  (.get buf))

(defn unpack-short [buf]
  (.getShort buf))

(defn unpack-int [buf]
  (.getInt buf))

(defn make-str-from-bytes [ba]
  (let [nba (into-array Byte/TYPE (map unchecked-byte (filter pos? (seq ba))))]
    (String. nba 0 (alength nba))))


(defn unpack-str [buf w]
  (let [bb (byte-array w)]
    (.get buf bb 0 w)
    (make-str-from-bytes bb)))

(def bb (->
          (ByteBuffer/allocate 32)
          (pack-byte 32)
          (pack-byte 1)
          (pack-byte 2)
          (pack-byte 3)
          (pack-byte -1)
          (pack-byte -138)
          (pack-byte 102)
          (pack-byte 0xff)

          (pack-short 32435)
          (pack-short 1024)
          (pack-short 32)
          (pack-short 0)

          (pack-int 0x12345678)
          (pack-int 0x87654321)

          (pack-str "hello" 4)
          (pack-str "hi" 4)

          (.flip)))

(with-open [o (output-stream "test.bin")]
  (.write o (.array bb)))

(println 
         [ (unpack-byte bb)
           (unpack-byte bb)
           (unpack-byte bb)
           (unpack-byte bb)
           (unpack-byte bb)
           (unpack-byte bb)
           (unpack-byte bb)
           (unpack-byte bb)
           (unpack-short bb)
           (unpack-short bb)
           (unpack-short bb)
           (unpack-short bb)
           (unpack-int bb)
           (unpack-int bb)
           (unpack-str bb 4)
           (unpack-str bb 4) ])




