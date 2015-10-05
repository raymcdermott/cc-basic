(ns car-configurator.suffix-parser
  (:refer-clojure :exclude [==])
  (:require [clara.rules.accumulators :as acc]
            [clara.rules :refer :all])
  (:import (clojure.lang PersistentHashSet)))

; get all the models / ssn / kata combinations

; get the suffixes for each model

; get the equipment for each suffix (configurable bill of materials)

; ok so now we have the configurable bill of materials for each suffix ...

; can we use Clara to detect the truth about the truth for each piece of
; equipment across the suffixes for this model?

; Clara accumulators are for testing collections

;(defrecord Transmission [transmission])

(defrecord Suffix [model engine transmission colour])

(defrule get-model-transmissions
         [?transmissions <- (acc/distinct :transmission) :from [Suffix]]
         =>
         (insert! ?transmissions))

(defquery check-transmissions []
          [?transmission <- PersistentHashSet])

(defn validate! [session]
  (doseq [result (query session check-transmissions)]
    (println "Transmission" (vals result)))
  session)

(defn run-examples []
  (let [session (-> (mk-session 'car-configurator.suffix-parser)
                    (insert (->Suffix :auris :diesel :automatic :black))
                    (insert (->Suffix :auris :diesel :manual :red))
                    (insert (->Suffix :auris :petrol :manual :black)))]
    (-> session
        (fire-rules)
        (validate!))))

(run-examples)

; gives
; Transmission (#{:manual :automatic})

; or...

(def auris #{{:model :auris :fuel :diesel :transmission :automatic :colour :black}
             {:model :auris :fuel :diesel :transmission :manual :colour :black}
             {:model :auris :fuel :diesel :transmission :automatic :colour :red}
             {:model :auris :fuel :diesel :transmission :manual :colour :red}})

(clojure.set/project auris [:transmission])

; gives
; #{{:transmission :automatic} {:transmission :manual}}

; so ... to just get the values

(map #(:transmission %) (clojure.set/project auris [:transmission]))

; gives
; (:automatic :manual)

; and so while the Clara approach works
; the Clojure approach seems more fitting - less code, less baggage
; and we are just looking for data in this case anyway that will eventually
; feed the rules engine

