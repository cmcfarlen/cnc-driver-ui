(ns cncui.bin
  (:require [clojure.java.io :as io])
  (:import [java.io FileInputStream Closeable]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]))

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

;; unsigned interpretation of signed data
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
  (end? [s])
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
  (put-byte ([buf v] (.put buf (unchecked-byte v)))
            ([buf v ofs cnt] (.put buf v ofs cnt)))
  (put-short [buf v] (.putShort buf (unchecked-short v)))
  (put-int [buf v] (.putInt buf (unchecked-int v)))
  (put-long [buf v] (.putLong buf (unchecked-long v)))
  (get-byte ([buf] (.get buf))
            ([buf ba ofs cnt] (.get buf ba ofs cnt)))
  (get-short [buf] (.getShort buf))
  (get-int [buf] (.getInt buf))
  (get-long [buf] (.getLong buf)))


(defrecord BinIOStream [in out order]
  BinStream
  (remaining? [buf] (> 0 (.available in)))
  (put-byte [buf v] (.write out (unchecked-int v)) buf)
  (put-byte [buf v ofs cnt] (.write out v ofs cnt) buf)
  (put-short [buf v] 
    (let [cv (unchecked-short v)]
      (if (= order :little)
        (do
          (put-byte buf cv)
          (put-byte buf (bit-shift-right cv 8)))
        (do
          (put-byte buf (bit-shift-right cv 8))
          (put-byte buf cv)))
      buf))
  (put-int [buf v]
    (let [cv (unchecked-int v)]
      (if (= order :little)
        (do
          (put-byte buf cv)
          (put-byte buf (bit-shift-right cv 8))
          (put-byte buf (bit-shift-right cv 16))
          (put-byte buf (bit-shift-right cv 24)))
        (do
          (put-byte buf (bit-shift-right cv 24))
          (put-byte buf (bit-shift-right cv 16))
          (put-byte buf (bit-shift-right cv 8))
          (put-byte buf cv)))
      buf))
  (put-long [buf v]
    (let [cv (unchecked-long v)]
      (if (= order :little)
        (do
          (put-byte buf cv)
          (put-byte buf (bit-shift-right cv 8))
          (put-byte buf (bit-shift-right cv 16))
          (put-byte buf (bit-shift-right cv 24))
          (put-byte buf (bit-shift-right cv 32))
          (put-byte buf (bit-shift-right cv 40))
          (put-byte buf (bit-shift-right cv 48))
          (put-byte buf (bit-shift-right cv 56)))
        (do
          (put-byte buf (bit-shift-right cv 56))
          (put-byte buf (bit-shift-right cv 48))
          (put-byte buf (bit-shift-right cv 40))
          (put-byte buf (bit-shift-right cv 32))
          (put-byte buf (bit-shift-right cv 24))
          (put-byte buf (bit-shift-right cv 16))
          (put-byte buf (bit-shift-right cv 8))
          (put-byte buf cv)))
      buf))
  (get-byte [buf] (unchecked-byte (.read in)))
  (get-byte [buf ba ofs cnt] (.read in ba ofs cnt))
  (get-short [buf] 
    (let [b1 (.read in)
          b2 (.read in)]
      (unchecked-short
        (if (= order :little)
          (bit-or
            (bit-shift-left b2 8)
            b1)
          (bit-or
            (bit-shift-left b1 8)
            b2)))))
  (get-int [buf]
    (let [b1 (.read in)
          b2 (.read in)
          b3 (.read in)
          b4 (.read in)]
      (unchecked-int
        (if (= order :little)
          (bit-or
            (bit-shift-left b4 24)
            (bit-shift-left b3 16)
            (bit-shift-left b2 8)
            b1)
          (bit-or
            (bit-shift-left b1 24)
            (bit-shift-left b2 16)
            (bit-shift-left b3 8)
            b4)))))
  (get-long [buf]
    (let [b1 (.read in)
          b2 (.read in)
          b3 (.read in)
          b4 (.read in)
          b5 (.read in)
          b6 (.read in)
          b7 (.read in)
          b8 (.read in)]
      (unchecked-long
        (if (= order :little)
          (bit-or
            (bit-shift-left b8 56)
            (bit-shift-left b7 48)
            (bit-shift-left b6 40)
            (bit-shift-left b5 32)
            (bit-shift-left b4 24)
            (bit-shift-left b3 16)
            (bit-shift-left b2 8)
            b1)
          (bit-or
            (bit-shift-left b1 56)
            (bit-shift-left b2 48)
            (bit-shift-left b3 40)
            (bit-shift-left b4 32)
            (bit-shift-left b5 24)
            (bit-shift-left b6 16)
            (bit-shift-left b7 8)
            b8))))))

(defn input-bin-stream
  "make an input bin stream"
  ([in]
   (input-bin-stream in :little))
  ([in order]
   (let [ins (io/input-stream in)]
     (->BinIOStream ins nil order))))

(defn output-bin-stream
  "make an output bin stream"
  ([out]
   (output-bin-stream out :little))
  ([out order]
   (let [outs (io/output-stream out)]
     (->BinIOStream nil outs order))))

(defn io-bin-stream
  [in out order]
  (->BinIOStream in out order))

(defn get-unsigned-byte
  [buf]
  (unsigned-byte (get-byte buf)))

(defn get-unsigned-short
  [buf]
  (unsigned-short (get-short buf)))

(defn get-unsigned-int
  [buf]
  (unsigned-int (get-int buf)))

(defn put-string [w buf s]
  (let [l (.length s)
        ba (.getBytes s)]
    (if (< l w)
      (-> buf
          (put-byte ba 0 l)
          (put-byte (byte-array (- w l)) 0 (- w l)))
      (put-byte buf (.getBytes s) 0 w))
    buf))

(defn make-string-from-bytes [ba]
  (let [nba (into-array Byte/TYPE (map unchecked-byte (filter pos? (seq ba))))]
    (String. nba 0 (alength nba))))

(defn get-string [w buf]
  (let [bb (byte-array w)]
    (get-byte buf bb 0 w)
    (make-string-from-bytes bb)))

(defn accum-size
  [total cnt esize e]
  (+ total (* cnt esize)))

(defn char->unpacker
  [e]
  (case e
    \b get-byte
    \B get-unsigned-byte
    \h get-short
    \H get-unsigned-short
    \i get-int
    \I get-unsigned-int
    \s get-string))

(defn char->packer
  [e]
  (do
    (case e
      (\b \B) put-byte
      (\h \H) put-short
      (\i \I) put-int
      \s put-string)))

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
        (reduce (fn [b [f e]] (f b e)) buf (partition 2 (interleave packers data)))))))

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

(defn empty-record
  "create an instance of binrecord with all null fields"
  [record-type]
  (let [ctr (resolve (symbol (str "/map->" (.getSimpleName record-type))))]
    (ctr {})))

(defn byte-buffer
  "utility to allocate a byte buffer"
  [sz]
  (ByteBuffer/allocate sz))

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

(defn byte-seq
  [buf]
  (lazy-seq (cons (get-byte buf) (byte-seq buf))))
