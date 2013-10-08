(ns cncui.bin-test
  (:import [java.nio ByteBuffer]
           [java.io File])
  (:require [clojure.java.io :as io])
  (:use clojure.test
        cncui.bin))

(def ^:dynamic *b* 1)

(defn myfixture [f]
  (binding [*b* (ByteBuffer/allocate 32)]
    (f)))

(use-fixtures :each myfixture)

(deftest packbytes
  (testing "packing byte"
    (let [d (range -127 127)]
      (is (= d (map (fn [b] (->> *b*
                            (.rewind)
                            (pack-byte! b)
                            (.flip)
                            (unpack-byte!))) d))))))

(deftest packubytes
  (testing "packing unsigned byte"
    (let [d (range 0 256)]
      (is (= d (map (fn [b] (->> *b*
                            (.rewind)
                            (pack-byte! b)
                            (.flip)
                            (unpack-unsigned-byte!))) d))))))

(deftest packshorts
  (testing "packing shorts"
    (let [d (range Short/MIN_VALUE Short/MAX_VALUE)]
      (is (= d (map (fn [v] (->> *b*
                            (.rewind)
                            (pack-short! v)
                            (.flip)
                            (unpack-short!))) d))))))

(deftest packushorts
  (testing "packing unsigned shorts"
    (let [d [0 10 Short/MAX_VALUE 65535]]
      (is (= d (map (fn [v] (->> *b*
                            (.rewind)
                            (pack-short! v)
                            (.flip)
                            (unpack-unsigned-short!))) d))))))

(deftest packints
  (testing "packing ints"
    (let [d [Integer/MIN_VALUE 0 Integer/MAX_VALUE]]
      (is (= d (map (fn [v] (->> *b*
                            (.rewind)
                            (pack-int! v)
                            (.flip)
                            (unpack-int!))) d))))))

(deftest packuints
  (testing "packing unsigned ints"
    (let [d [0 Integer/MAX_VALUE 4000000000 4294967295]]
      (is (= d (map (fn [v] (->> *b*
                            (.rewind)
                            (pack-int! v)
                            (.flip)
                            (unpack-unsigned-int!))) d))))))


(deftest packstrings
  (testing "packing strings"
    (let [s ["a" "b" "hello" "world"]]
      (is (= s (map (fn [v] (->> *b*
                            (.rewind)
                            (pack-string! 32 v)
                            (.flip)
                            (unpack-string! 32))) s))))))


(defn make-binfile
  []
  (let [tmpf (File/createTempFile "test" "bin")
        bb   (byte-buffer 32)]
    (->> bb
        (pack-int! 1)
        (pack-int! 2)
        (pack-int! 3)
        (pack-int! 4)
        (pack-int! 5)
        (pack-int! 6)
        (pack-int! 7)
        (pack-int! 8)
        (flip))
    (with-open [ostr (io/output-stream tmpf)]
      (doall (take 10 (repeatedly (fn [] (.write ostr (.array bb)))))))
    tmpf))

(defn make-tmpout
  []
  (let [tmpf (File/createTempFile "testout" "bin")]
    (io/output-stream tmpf)))



(deftest biniostream
  (testing "bininput biniostrem"
    (let [bf (make-binfile)]
      (with-open [in (io/input-stream bf)]
        (let [bs (->BinIOStream in nil :big)
              istr (take 8 (repeatedly (fn [] (unpack-int! bs))))
              exp [1 2 3 4 5 6 7 8]]
          (is (= istr exp))))))
  (testing "binoutput biniostream"
    (let [tmpf (File/createTempFile "testout" "bin")
          os (io/output-stream tmpf)
          bs (->BinIOStream nil os :big)]
      (-> bs
          (put-int 1)
          (put-short 2)
          (put-byte 3)
          (:out)
          (.close))
      (is (= (.length tmpf) 7))
      (let [ins (io/input-stream tmpf)
            bs (assoc bs :in ins)
            d  [(get-int bs) (get-short bs) (get-byte bs)]
            e  [1 2 3]]
        (is (= d e))))))

(defbinrecord TestRecord "bhiBHIs10" [ sb ss si ub us ui s ])

(deftest binrecords
  (testing "binrecord identity"
    (let [tr (->TestRecord 100 10000 1000000 250 50000 3000000000 "1234567890")
          bb (byte-buffer (binsize tr))]
      (is (= (:sb tr) 100))
      (is (= (:ss tr) 10000))
      (is (= (:si tr) 1000000))
      (is (= (:ub tr) 250))
      (is (= (:us tr) 50000))
      (is (= (:ui tr) 3000000000))
      (is (= (:s tr) "1234567890"))
      (pack tr bb)
      (is (= (.position bb) (binsize tr)))
      (flip bb)
      (let [unpacked-tr (unpack tr bb)]
        (is (= (:sb unpacked-tr) 100))
        (is (= (:ss unpacked-tr) 10000))
        (is (= (:si unpacked-tr) 1000000))
        (is (= (:ub unpacked-tr) 250))
        (is (= (:us unpacked-tr) 50000))
        (is (= (:ui unpacked-tr) 3000000000))
        (is (= (:s  unpacked-tr) "1234567890")))
      )))

(defbinrecord TestRecord2 "iiiiiiii" [o t th f fi s se e])

(deftest binfile
  (testing "making a binary file"
    (let [binf (make-binfile)]
      (is (.exists binf))
      (is (= (.length binf) 320))))
  (testing "reading records from binary file"
    (let [binf (make-binfile)
          trec (->TestRecord2 0 0 0 0 0 0 0 0)
          bb (map-file binf)
          records (take 10 (repeatedly (fn [] (unpack trec bb))))]
      (printf (.getAbsolutePath binf))
      (is (= (count records) 10))
      (map (fn [e] (is (= (:o e) 1))) records))))



