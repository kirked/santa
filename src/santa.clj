(ns santa
  (:require
    [santa.twilio :as twilio]))

(defn mapping
  "Randomly choose giver-receiver pairs and return their indices in a map."
  [participants]
  (letfn [(grab [items]
            (-> items seq rand-nth))]
    (let [n (count participants)]
      (loop [m      {}
             givers (zipmap (range n) participants)
             takers givers]
      (if (empty? givers)
        m
        (let [[g _] (grab givers)
              [t  ] (first (drop-while #(= g (first %)) takers))]
          (if (not= g t)
            (recur (assoc m g t)
                   (dissoc givers g)
                   (dissoc takers t))
            (recur m givers takers))))))))

(defn format-text
  "Format the text message."
  [{:keys [name address city state zip]}]
  (format "You are secret Santa for %s. Send to %s, %s, %s %s"
          name address city state zip))

(defn text-to
  "Return text message info for a participant pair."
  [from to]
  (merge {:message (format-text to)}
         (select-keys from [:name :phone])))

(defn texts
  "Given participants and the selection mapping, return a vector
   of text messages."
  [participants selections]
  (reduce (fn [v [g t]]
            (let [giver (nth participants g)
                  taker (nth participants t)]
              (conj v (text-to giver taker))))
          []
          selections))

(defn show
  [participants selections]
  (letfn [(show-pair [g t]
            (let [giver (get-in participants [g :name])
                  taker (get-in participants [t :name])]
              (format "%-20s => %s" giver taker)))]
    (doseq [[g t] selections]
      (println (show-pair g t)))))


(defn send-texts!
  [texts]
  (doseq [{:keys [message phone]} texts]
    (twilio/send-text! phone message)))
