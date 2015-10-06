(ns car-configurator.min-parser
  (:refer-clojure :exclude [==])
  (:require [clojure.set :as set]
            [clara.rules.accumulators :as acc]
            [clara.rules :refer :all]))

(defrecord InvalidOption [field message])

(defrecord ValidFuel [fuel])

(defrecord Suffix [model fuel transmission colour])

; this is what could come from a DB
(def somename-suffixes [{:model :somename :fuel :diesel :transmission :automatic :colour :black}
                        {:model :somename :fuel :diesel :transmission :manual :colour :black}
                        {:model :somename :fuel :petrol :transmission :manual :colour :black}
                        {:model :somename :fuel :diesel :transmission :automatic :colour :red}
                        {:model :somename :fuel :diesel :transmission :manual :colour :red}])

; base data as 'good' data into the fact space
(def suffixes (map #(->Suffix (:model %)
                              (:fuel %)
                              (:transmission %)
                              (:colour %)) somename-suffixes))

(def valid-fuels (set (map #(:fuel %) somename-suffixes)))

;(def colours (map #(->Colour (:model %) (:colour %)) somename-suffixes))

;(def transmissions (map #(->Transmission (:model %) (:transmission %)) somename-suffixes))

;(defrule valid-fuel
;         "Transmissions must be in the allowed set"
;         [?suffix <- Suffix (== ?model model) (== ?fuel fuel)]
;         [:test (not (contains? possible-fuels ?fuel))]
;         =>
;         (println "fail on" ?fuel "for model" ?model "on suffix" ?suffix))
;
;(defrule valid-colours
;         "Transmissions must be in the allowed set"
;         [?suffix <- Suffix (not (contains? possible-colours colour))
;          (== ?model model)
;          (== ?colour colour)]
;         =>
;         (println "fail on" ?colour "for model" ?model "on suffix" ?suffix))
;
;(defrule valid-transmission
;         "Transmissions must be in the allowed set"
;         [?suffix <- Suffix (not (contains? possible-transmissions transmission))
;          (== ?model model)
;          (== ?transmission transmission)]
;         =>
;         (println "fail on" ?transmission "for model" ?model "on suffix" ?suffix))

(defrule check-valid-fuel
         "Inserts an InvalidOption fact if the model presents an invalid fuel."
         [?fuel-count <- (acc/distinct :fuel) :from [Suffix (== ?fuel fuel)]]
         [:test (not (empty? (set/difference ?fuel-count valid-fuels)))]
         =>
         (println "fails on" ?fuel))

(defn run-examples []
  (let [session (-> (mk-session 'car-configurator.min-parser)
                    (insert-all suffixes)
                    (insert-all valid-fuels)
                    ; and a bad one
                    (insert (->Suffix :somename
                                      :jibberish
                                      :manual
                                      :red)))]
    (time (fire-rules session))))

(run-examples)

; and this seems nicer

