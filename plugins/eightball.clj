(ns plugins.eightball)

(defn eightball [arg]
  (let [ansr ["Ask Again Later"
              "Better Not Tell You Now"
              "Concentrate and Ask Again"
              "Don't Count on It"
              "It Is Certain"
              "Most Likely"
              "My Reply is No"
              "My Sources Say No"
              "No"
              "Outlook Good"
              "Outlook Not So Good"
              "Reply Hazy, Try Again"
              "Signs Point to Yes"
              "Yes"
              "Yes, Definitely"
              "You May Rely On It"]]
    {:payload (nth ansr (rand-int (count ansr)))
     :to (:to arg)}))

(def properties {:name "eightball"
                 :matches ["8ball"]
                 :dispatch eightball})
