(ns cncui.bin
  (:use [clojure.java.io])
  (:import [java.io FileInputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]))

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

(defn unsigned-byte
  "interprets a byte as unsigned and returns as an int"
  [v]
  (let [vv (unchecked-byte v)] (if (< vv 0) (+ 256 vv) vv)))

(defn unsigned-short
  "interprets a short as unsigned and returns as an int"
  [v]
  (let [vv (unchecked-short v)] (if (< vv 0) (+ 65536 vv) vv)))

(defn unsigned-int [v]
  "interprets a int as unsigned and returns as an long"
  (let [vv (unchecked-int v)] (if (< vv 0) (+ 4294967296 vv) vv)))

(defprotocol BinStream
  "Protocol with an interface like bytebuffer for writing binary data to a stream
   io devices extending this protocol should be able to pack straight from the device"
  (remaining? [s])
  (flip [s])
  (put-byte [s v] [s v ofs cnt] "could be a byte or byte[]")
  (put-short [s v])
  (put-int [s v])
  (put-long [s v])
  (get-byte [s] [s ba ofs cnt])
  (get-short [s])
  (get-int [s])
  (get-long [s]))

; ByteBuffer extension
(extend-protocol BinStream
  ByteBuffer
  (remaining? [buf] (.hasRemaining buf))
  (flip [buf] (.flip buf))
  (put-byte ([buf v] (.put buf v))
            ([buf v ofs cnt] (.put buf v ofs cnt)))
  (put-short [buf v] (.putShort buf v))
  (put-int [buf v] (.putInt buf v))
  (put-long [buf v] (.putLong buf v))
  (get-byte ([buf] (.get buf))
            ([buf ba ofs cnt] (.get buf ba ofs cnt)))
  (get-short [buf] (.getShort buf))
  (get-int [buf] (.getInt buf))
  (get-long [buf] (.getLong buf)))


(defn pack-byte! [v buf]
  (put-byte buf (unchecked-byte (bit-and 0xff v)))
  buf)

(defn pack-short! [v buf]
  (put-short buf (unchecked-short (bit-and 0xffff v)))
  buf)

(defn pack-int! [v buf]
  (put-int buf (unchecked-int (bit-and 0xffffffff v)))
  buf)

(defn pack-string! [w s buf]
  (let [l (.length s)]
    (if (< l w)
      (-> buf
          (put-byte (.getBytes s))
          (put-byte (byte-array (- w l))))
      (put-byte buf (.getBytes s) 0 w))
    buf))

(defn unpack-byte! [buf]
  (get-byte buf))

(defn unpack-short! [buf]
  (get-short buf))

(defn unpack-int! [buf]
  (get-int buf))

(defn unpack-unsigned-byte! [buf]
  (unsigned-byte (unpack-byte! buf)))

(defn unpack-unsigned-short! [buf]
  (unsigned-short (unpack-short! buf)))

(defn unpack-unsigned-int! [buf]
  (unsigned-int (unpack-int! buf)))

(defn make-string-from-bytes [ba]
  (let [nba (into-array Byte/TYPE (map unchecked-byte (filter pos? (seq ba))))]
    (String. nba 0 (alength nba))))

(defn unpack-string! [w buf]
  (let [bb (byte-array w)]
    (get-byte buf bb 0 w)
    (make-string-from-bytes bb)))


(defn accum-size
  [total cnt esize e]
  (+ total (* cnt esize)))

(defn char->unpacker
  [e]
  (case e
    \b unpack-byte!
    \B unpack-unsigned-byte!
    \h unpack-short!
    \H unpack-unsigned-short!
    \i unpack-int!
    \I unpack-unsigned-int!
    \s unpack-string!))

(defn char->packer
  [e]
  (do
    (case e
      (\b \B) pack-byte!
      (\h \H) pack-short!
      (\i \I) pack-int!
      \s pack-string!)))

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
  ([s]
   (let [sz (struct-size s)]
     (struct-packer s (ByteBuffer/allocate sz))))
  ([s buf]
    (let [packers (struct-reduce (partial accum-marshal char->packer) [] s)]
      (fn [data]
        (reduce (fn [b [f e]] (f e b)) buf (partition 2 (interleave packers data)))))))

(defprotocol BinaryRecord
  (binsize [r])
  (pack [r] [r buf])
  (unpack [r buf])
  (signature [r]))

(defmacro defbinrecord
  [name signature fields]
  `(defrecord ~name ~fields
     BinaryRecord
     (binsize [_] (struct-size ~signature))
     (pack [_] ((struct-packer ~signature) ~fields))
     (pack [_ buf#] ((struct-packer ~signature buf#) ~fields))
     (unpack [r# buf#]
       (let [d# ((struct-unpacker ~signature) buf#)
             kw# (interleave (keys r#) d#)]
         (apply (partial assoc r#) kw#)))
     (signature [_] ~signature)))

(defn record-seq
  [rec buf]
  (if (remaining? buf)
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


;(defbinrecord TestPacket "hhibhs5" [magic length test-int test-byte test-short test-str])
;(def s (read-records "out.dat" (TestPacket. 1 1 1 1 1 "")))

;(defbinrecord Testr "bbbb" [a b c d])
;(def t (->Testr 1 1 1 1))
;(def b (pack t))
;(println (unpack t (flip b)))

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
