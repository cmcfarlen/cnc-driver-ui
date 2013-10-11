(ns cncui.mill
    (:require [cncui.message :as msg]
              [cncui.core :as core]
              [cncui.bin :as bin]))


; message definitions
(bin/defbinrecord PingMessage "b" [status])
(bin/defbinrecord ErrorMessage "s" [status])
(bin/defbinrecord InfoMessage "s" [status])
(bin/defbinrecord TestMessage "is16" [counter message])
(bin/defbinrecord StartMessage "i" [velocity])
(bin/defbinrecord StopMessage "i" [r])
(bin/defbinrecord SetupMessage "i" [r])

(def mill-messages
  [cncui.mill.PingMessage 0
   cncui.mill.ErrorMessage 1
   cncui.mill.InfoMessage 2
   cncui.mill.TestMessage 16
   cncui.mill.StartMessage 17
   cncui.mill.StopMessage 18
   cncui.mill.SetupMessage 19])

; high level protocol
(defprotocol Mill
  (stop! [m])
  (move! [m [xdir xvel xacc xdist] [ydir yvel yacc ydist] [zdir zvel zacc zdist]])
  (move-x! [m [xdir xvel xacc xdist]])
  (move-y! [m [ydir yvel yacc ydist]])
  (move-z! [m [zdir zvel zacc zdist]]))


(defrecord MillService [msg-svc]
  core/Service
  (start [svc]
    (core/notify "Starting mill service")
    (reduce (fn [_ [tp id]] (msg/register-message msg-svc tp id)) nil (partition 2 mill-messages))
    svc)
  (stop [svc]
    (core/notify "Stopping mill service")
    svc))

(defn mill-service [msg-svc]
  (->MillService msg-svc))

