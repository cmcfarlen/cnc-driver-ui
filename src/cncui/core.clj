(ns cncui.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [seesaw.core :as ss])
  (:import [java.io PushbackReader]))

(def default-config
  { :dev "/dev/tty.usbserial" })

(defn tty-devices []
  (filter #(.startsWith (.getName %) "tty.") (file-seq (io/file "/dev"))))

(defn config-ui-axis
  [id label]
  [ id label
    {:id :mode :label "Mode" :type :number :range (range 1 8) :default 8}
    {:id :pitch :label "Pitch" :type :number :default 8}])

(def config-ui
  [{:id :dev :label "Device" :type :path :model tty-devices}
    [:limits "Limits"
      {:id :freq :label "Frequency" :type :number :range (range 0 1000000)}]
    [:driver "Driver"
      (config-ui-axis :x-axis "X-Axes")
      (config-ui-axis :y-axis "Y-Axes")
      (config-ui-axis :z-axis "Z-Axes")]])

(declare widget-from-map)

(defn component-for
  [e n]
  (cond
    (map? e) (ss/vertical-panel :id n :border (name n) :items (widget-from-map e))
    (number? e) (ss/horizontal-panel :items [(name n) (ss/spinner :id n :model e)])
    :else       (ss/horizontal-panel :items [(name n) (ss/text :id n :text e)])))

(defn widget-from-map
  [m]
  (map (fn [[a b]] (component-for b a)) (seq m)))

(defn build-config-widget
  [cfg]
  (ss/vertical-panel
    :items (widget-from-map cfg)))

(defn config-save-clicked
  [e]
  (ss/alert e "Clicked"))
    
    
    ;[ "Device" (ss/selection! 
    ;                    (ss/combobox :id :dev :model (tty-devices))
    ;                    (io/as-file (:dev cfg)))]))

(defn config-dialog
  "Create a config dialog"
  [cfg]
  (-> (ss/frame :title "Configuration"
                :on-close :dispose
                :content 
                (ss/border-panel :center (build-config-widget cfg)
                                 :south (ss/flow-panel :align :right :items 
                                                       [(ss/action :name "Save" :handler config-save-clicked)])))
      (ss/pack!)
      (ss/show!)))

(defn load-config []
  (let [cfg (io/as-file (or (System/getenv "CNCUI_CONFIG") (str (System/getenv "HOME") "/.cncui")))
        exists (.exists cfg)]
    (if exists
      (edn/read (PushbackReader. (io/reader cfg)))
      {})))

(def config (atom (load-config)))

(defn -main
  "I don't do a whole lot."
  [& args]
  (let [cfg @config]
    (pr cfg)
    (ss/native!)
    (config-dialog cfg)))
