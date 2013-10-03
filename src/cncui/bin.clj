(ns cncui.bin
  (:use [clojure.java.io])
  (:import [java.io FileInputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]))

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

(defn pack-string [w buf s]
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

(defn unpack-string [w buf]
  (let [bb (byte-array w)]
    (.get buf bb 0 w)
    (make-string-from-bytes bb)))


(defn pack-my-struct [values buf]
  (reduce (fn [_ [f v]] (f buf v)) nil (map vector [pack-byte pack-byte pack-byte] values)))


(defn accum-size
  [total cnt esize e]
  (+ total (* cnt esize)))

(defn char->unpacker
  [e]
  (case e
    \b unpack-byte
    \B unpack-unsigned-byte
    \h unpack-short
    \H unpack-unsigned-short
    \i unpack-int
    \I unpack-unsigned-int
    \s unpack-string))

(defn char->packer
  [e]
  (do
    (case e
      (\b \B) pack-byte
      (\h \H) pack-short
      (\i \I) pack-int
      \s pack-string)))

(defn char->size
  [e]
  (case e
    (\b \B \s) 1
    (\h \H) 2
    (\i \I) 4))

(defn accum-marshal
  [mapper marshallers cnt esize e]
  (if (nil? e)
    marshallers
    (let [marshaller (mapper e)]
      (if (= e \s)
        (conj marshallers (partial marshaller cnt))
        (if (= 1 cnt)
          (conj marshallers marshaller)
          (concat marshallers (repeat cnt marshaller)))))))

(defn dig
  [d]
  (- (int d) (int \0)))

(defn struct-reducer
  [accumf [state cnt esize e] ch]
  (case ch
    (\b \B \h \H \i \I \s) [(accumf state cnt esize e) 0 (char->size ch) ch]
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) [state (+ (dig ch) (* 10 cnt)) esize e]))

(defn wrap-accum
  [afn]
  (fn [s cnt es e]
    (afn s (max 1 cnt) es e)))

(defn struct-reduce
  [accumfn istate s]
  (let [afn (wrap-accum accumfn)]
    (apply afn (reduce (partial struct-reducer afn) [istate 0 0 nil] s))))

(defn struct-size
  [s]
  (struct-reduce accum-size 0 s))

(defn struct-unpacker
  [s]
  (let [sz (struct-size s)
        unpackers (struct-reduce (partial accum-marshal char->unpacker) [] s)]
    (fn [buf]
      (map (fn [f] (f buf)) unpackers))))

(defn struct-packer
  [s]
  (let [sz (struct-size s)
        packers (struct-reduce (partial accum-marshal char->packer) [] s)]
    (fn [data]
      (let [buf (ByteBuffer/allocate sz)]
        (.flip (reduce (fn [b [f e]] (f b e)) buf (partition 2 (interleave packers data))))))))

(defprotocol BinaryRecord
  (binsize [r])
  (pack [r])
  (unpack [r buf])
  (signature [r]))

(defmacro defbinrecord
  [name signature fields]
  `(defrecord ~name ~fields
     BinaryRecord
     (binsize [_] (struct-size ~signature))
     (pack [_] ((struct-packer ~signature) ~fields))
     (unpack [r# buf#]
       (let [d# ((struct-unpacker ~signature) buf#)
             kw# (interleave (keys r#) d#)]
         (apply (partial assoc r#) kw#)))
     (signature [_] ~signature)))

(defn record-seq
  [rec buf]
  (if (.hasRemaining buf)
    (cons (unpack rec buf) (record-seq rec buf))
    []))

(defn map-file
  [file]
  (let [is (FileInputStream. file)
        ch (.getChannel is)]
    (.order (.map ch FileChannel$MapMode/READ_ONLY 0 (.size ch)) ByteOrder/LITTLE_ENDIAN)))


(defn read-records
  [file rec]
    (record-seq rec (map-file file)))


(defbinrecord TestPacket "hhibhs5" [magic length test-int test-byte test-short test-str])
(def s (read-records "out.dat" (->TestPacket 1 1 1 1 1 "")))

;(defbinrecord Testr "bbbb" [a b c d])
;(def t (->Testr 1 1 1 1))
;(def b (pack (->Testr 4 3 2 1)))
;(println (unpack t b))

;; (defn defbinrecord
;;   [name spec & fields]
;;   (let [sz (struct-size spec)
;;         packer (struct-packer spec)
;;         unpacker (struct-unpacker spec)]
;;     (defrecord name fields
;;       BinaryRecord
;;       (size [_] sz)
;;       (pack [r d] (packer r))
;;       (unpack [r b] (unpacker b)



