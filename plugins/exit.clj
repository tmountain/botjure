(ns plugins.exit)
(declare exit)

(def admins ["jjames" "tmountain"])

(defn do-exit [status]
  (shutdown-agents)
  (flush)
  (System/exit status))
 
(defn exit [arg]
  (if (filter #(= %1 (:from arg)) admins)
    (do-exit 0)))

(def properties {:name      "exit",
                 :matches   ["exit"],
                 :dispatch  exit })
