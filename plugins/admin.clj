(ns plugins.admin)

(def admins ["jjames" "tmountain"])

;; This should call the disconnect function but
;; we don't have access to the socket to call it
;; directly.
(defn exit [status]
  (shutdown-agents)
  (flush)
  (System/exit status))

;; No support currently for non-privmsg commands
;; (defn opme [user]
;;   {:admin (str "MODE " (:to arg) " +o " (:from arg))})

(defn admin [arg]
  (if (filter #(= %1 (:from arg)) admins)
    (cond (= (:cmd arg) "exit") (exit 0)
          ;; (= cmd "opme") (opme (:from arg)
          )))

(def properties {:name      "admin",
                 :matches   :all,
                 :dispatch  admin })
