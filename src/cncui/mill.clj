(ns cncui.mill
    (:require [cncui.message :as msg]
              [cncui.core :as core]
              [cncui.bin :as bin]))


; message definitions
(bin/defbinrecord PingMessage "b" [status])
(bin/defbinrecord ErrorMessage "s" [status])
(bin/defbinrecord InfoMessage "s" [status])
(bin/defbinrecord TestMessage "is16" [counter message])

(def mill-messages
  [cncui.mill.PingMessage 0
   cncui.mill.ErrorMessage 1
   cncui.mill.InfoMessage 2
   cncui.mill.TestMessage 16])

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
    (msg/register-message msg-svc PingMessage 0)
    (msg/register-message msg-svc ErrorMessage 1)
    (msg/register-message msg-svc InfoMessage 2)
    (msg/register-message msg-svc TestMessage 16)
    svc)
  (stop [svc]
    (core/notify "Stopping mill service")
    svc))

(defn mill-service [msg-svc]
  (->MillService msg-svc))

