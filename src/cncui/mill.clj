(ns cncui.mill
    (:require [cncui.message :as msg]
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


