(ns cncui.bin
  (:use [clojure.java.io])
  (:import [java.nio ByteBuffer]))

(defn unsigned-byte [v]
  (let [vv (unchecked-byte v)] (if (< vv 0) (+ 256 vv) vv)))

(defn unsigned-short [v]
  (let [vv (unchecked-short v)] (if (< vv 0) (+ 65536 vv) vv)))

(defn unsigned-int [v]
  (let [vv (unchecked-int v)] (if (< vv 0) (+ 4294967296 vv) vv)))

(defn pack-byte [buf v]
  (.put buf (unchecked-byte (bit-and 0xff v)))
  buf)

(defn pack-short [buf v]
  (.putShort buf (unchecked-short (bit-and 0xffff v))))

(defn pack-int [buf v]
  (.putInt buf (unchecked-int (bit-and 0xffffffff v))))

(defn pack-string [buf s w]
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

(defn unpack-unsigned-byte [buf]
  (unsigned-byte (unpack-byte buf)))

(defn unpack-unsigned-short [buf]
  (unsigned-short (unpack-short buf)))

(defn unpack-unsigned-int [buf]
  (unsigned-int (unpack-int buf)))

(defn make-string-from-bytes [ba]
  (let [nba (into-array Byte/TYPE (map unchecked-byte (filter pos? (seq ba))))]
    (String. nba 0 (alength nba))))

(defn unpack-string [buf w]
  (let [bb (byte-array w)]
    (.get buf bb 0 w)
    (make-string-from-bytes bb)))


(defn pack-my-struct [values buf]
  (reduce (fn [_ [f v]] (f buf v)) nil (map vector [pack-byte pack-byte pack-byte] values)))


(defn struct-size
  [s]
  (reduce (fn [t c] (+ t
                       (case c
                         \b 1
                         \h 2
                         \i 4))) 0 (seq s)))


