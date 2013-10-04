(ns cncui.bin-test
  (:import [java.nio ByteBuffer])
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


