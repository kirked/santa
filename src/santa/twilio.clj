(ns santa.twilio
  (:require
    [cheshire.core :as json]
    [clj-http.client :as http]
    [clojure.edn :as edn]
    [clojure.string :as string]))


(def ^:private twilio
  (delay
    (-> (slurp "resources/twilio.edn") edn/read-string)))

(defn- twilio-endpoint
  [suffix]
  (str (:endpoint @twilio) "/Accounts/" (:client-id @twilio) suffix))

(defn format-phone
  "Format a phone number for SMS."
  [phone]
  (as-> phone $
      (string/replace $ #"-" "")
      (str "+1" $)))

(defn format-message
  "Format the body of the send-text API call."
  [from-phone to-phone message]
  {:From (format-phone from-phone)
   :To   (format-phone to-phone)
   :Body message})

(defn send-text!
  "Send a text message from a registered phone number to another."
  [to-phone message & [from-phone]]
  (if-let [from-phone (or from-phone (:from-phone @twilio))]
    (->> {:accept           :json
          :basic-auth       [(:client-id @twilio) (:secret @twilio)]
          :form-params      (format-message from-phone to-phone message)
          :throw-exceptions false}
         (http/post (twilio-endpoint "/Messages.json"))
         (:body)
         (json/parse-string))
    (throw (IllegalArgumentException. "No from-phone provided"))))
