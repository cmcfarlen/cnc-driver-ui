(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.repl :refer :all]
            [clojure.test :as test]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [cncui.bin :as bin]
            [cncui.port :as port]
            [cncui.core :as core]
            [cncui.message :as msg]
            [cncui.mill :as mill]))

(defn hey
  []
  (println "heyheyhey"))


(defn make-data
  [cnt]
  (let [file "make-data.bin"
        bs (bin/output-bin-stream file)
        msvc (msg/message-service bs)
        t  (mill/->TestMessage 0 "helloworld")]
    (core/start msvc)
    (msg/register-message msvc cncui.mill.TestMessage 16)
    (reduce (fn [_ m] (msg/write-message msvc m)) nil (map #(assoc t :count %1) (range cnt)))
    (core/stop msvc)
    (.close (:out bs))))

(defn make-mseq
  [vfn]
  (let [file "make-data.bin"
        bs (bin/input-bin-stream file)
        msvc (msg/message-service bs)
        t  (mill/->TestMessage 0 "helloworld")]
    (core/start msvc)
    (msg/register-message msvc cncui.mill.TestMessage 16)
    (vfn (msg/message-seq msvc))
    (core/stop msvc)
    (.close (:in bs))))



