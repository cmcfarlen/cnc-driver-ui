(ns cncui.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [seesaw.core :as ss])
  (:import [java.io PushbackReader]))

(defprotocol Service
  (start [svc])
  (stop  [svc]))

(defn notify
  [& msgs]
  (apply (partial println "NOTIFY:") msgs))


(def default-config
  { :dev "/dev/tty.usbserial" })

(defn tty-devices []
  (filter #(.startsWith (.getName %) "tty.") (file-seq (io/file "/dev"))))

(defn config-ui-axis
  [id label]
  [ id label
    {:id :mode :label "Mode" :type :number :range (range 0 9) :default 1}
    {:id :pitch :label "Pitch" :type :number :range (range 0 9) :default 1}])

(def config-ui
  [{:id :dev :label "Device" :type :select :model (tty-devices)}
    [:limits "Limits"
      {:id :freq :label "Frequency" :type :number :range (range 0 10000)}]
    [:driver "Driver"
      (config-ui-axis :x-axis "X-Axis")
      (config-ui-axis :y-axis "Y-Axis")
      (config-ui-axis :z-axis "Z-Axis")
      (config-ui-axis :a-axis "A-Axis")]])

(defn default-config
  "Create a default config from a config description"
  [cfg]
  (reduce (fn [s e] 
            (cond
              (map? e) (assoc s (:id e) (or (:default e) "unk"))
              (vector? e) (assoc s (first e) (default-config (drop 2 e))))) {} cfg))

(declare widgets-from-data)

(defn component-from-map
  [m]
  (case (:type m)
    :number (ss/spinner :id (name (:id m)) :model (:range m))
    :select (ss/combobox :id (name (:id m)) :model (:model m))
    :input  (ss/text :id (name (:id m)) :text (:default m))))

(defn component-for
  [d]
  (cond
    (map? d) (ss/horizontal-panel :items [(:label d) (component-from-map d)])
    (vector? d) (ss/vertical-panel :id (first d) :border (second d) :items (widgets-from-data (drop 2 d)))))

(defn widgets-from-data
  "Return a vector of widgets for the given vector of definitions"
  [v]
  (reduce (fn [col n] (conj col (component-for n))) [] v))

(defn build-config-widget
  [cfg]
  (do
    (ss/vertical-panel
      :items (widgets-from-data cfg))))

(defn fill-dialog
  [component cfgdef config]
  (reduce (fn [_ e]
            (cond
              (map? e) 
                 (let [selectkw (keyword (str "#" (name (:id e))))
                       cfgval   (get config (:id e))
                       cmp      (ss/select component [selectkw])]
                   (println selectkw cfgval cmp)
                   (ss/selection! cmp cfgval))
              (vector? e)
                 (let [selectkw (keyword (str "#" (name (first e))))]
                   (fill-dialog (ss/select component [selectkw]) (drop 2 e) (get config (first e)))))) nil cfgdef)
  component)


(defn config-save-clicked
  [e]
  (ss/alert e "Clicked"))

(defn load-config []
  (let [cfg (io/as-file (or (System/getenv "CNCUI_CONFIG") (str (System/getenv "HOME") "/.cncui")))
        exists (.exists cfg)]
    (if exists
      (edn/read (PushbackReader. (io/reader cfg)))
      {})))

(def config (atom (load-config)))


    
    ;[ "Device" (ss/selection! 
    ;                    (ss/combobox :id :dev :model (tty-devices))
    ;                    (io/as-file (:dev cfg)))]))

(defn config-dialog
  "Create a config dialog"
  [cfg]
  (-> (ss/frame :title "Configuration"
                :on-close :dispose
                :content 
                (ss/border-panel :center (build-config-widget config-ui)
                                 :south (ss/flow-panel :align :right :items 
                                                       [(ss/action :name "Save" :handler config-save-clicked)])))
      (fill-dialog config-ui cfg)
      ss/pack!
      ss/show!))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "hey!"))

