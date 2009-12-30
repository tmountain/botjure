(ns plugins.admin
  (:use plugin-helper))

(def admins ["jjames" "tmountain"])

;; This should call the disconnect function but
;; we don't have access to the socket to call it
;; directly.
(defn exit [status]
  (shutdown-agents)
  (flush)
  (System/exit status))

(defn opme [user channel]
  {:payload (str "MODE " channel " +o " user)
   :method srvmsg})

(defn admin [arg]
  (let [cmd (:cmd arg) user (:from arg)]
    (if (filter #(= %1 user) admins)
      (cond (= cmd "exit")
            (exit 0)
            (= cmd "opme")
            (opme user (:to arg))))))

(def properties {:name      "admin",
                 :matches   ["exit" "opme"],
                 :dispatch  admin })
