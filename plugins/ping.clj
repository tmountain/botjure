(ns plugins.ping
  (:use plugin-helper))

(defn ping [arg]
  (if (= (:msgtype arg) :server)
    {:payload (str "PONG " (.substring (:msg arg) 1))
     :method srvmsg}))

(def properties {:name "ping"
                 :matches ["PING"]
                 :dispatch ping})