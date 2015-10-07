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
(def valid-colours (set (map #(:colour %) somename-suffixes)))
(def valid-transmissions (set (map #(:transmission %) somename-suffixes)))


(defrule check-valid-fuel
         "Inserts an InvalidOption fact if the model presents an invalid fuel."
         [?fuel-count <- (acc/distinct :fuel) :from [Suffix]]
         [:test (not (empty? (set/difference ?fuel-count valid-fuels)))]
         =>
         (println "fails on" (set/difference ?fuel-count valid-fuels)))

(defn run-examples []
  (let [session (-> (mk-session 'car-configurator.min-parser)
                    (insert-all suffixes)
                    (insert-all valid-fuels)
                    ; and a bad one
                    (insert (->Suffix :somename :jibberish :manual :red)))]
    (comment time (fire-rules session))))

(run-examples)

(defn valid-option [key]
  (acc/distinct key))

(defn partial= [collection]
  (partial = collection))

(def partial-fuel (valid-option :fuel))
(def partial-eq-fuels (partial= valid-fuels))

(defn run-examples2 []
  (let [rules '[; Rules structure must be quoted so Clara can evaluate it...

                {:doc  "This is the same rule as check-valid-fuel above but defined as data"
                 :lhs  [{:result-binding :?presented-options
                         :accumulator    partial-fuel
                         :from           {:type        car_configurator.min_parser.Suffix
                                          :constraints []}}
                        {:constraints [(not (partial-eq-fuels ?presented-options))]}]
                 :rhs  (println "Fails on x:" (set/difference ?presented-options valid-fuels))}

                {:lhs  [{:result-binding :?presented-options
                         :accumulator    (acc/distinct :colour)
                         :from           {:type        car_configurator.min_parser.Suffix
                                          :constraints []}}
                        {:constraints [(not (empty? (set/difference ?presented-options valid-colours)))]}]
                 :rhs  (println "Fails on y:" (set/difference ?presented-options valid-colours))}

                {:lhs  [{:result-binding :?presented-options
                         :accumulator    (acc/distinct :transmission)
                         :from           {:type        car_configurator.min_parser.Suffix
                                          :constraints []}}
                        {:constraints [(not (empty? (set/difference ?presented-options valid-transmissions)))]}]
                 :rhs  (println "Fails on z:" (set/difference ?presented-options valid-transmissions))}
                ]
        session (-> (mk-session rules)
                    (insert-all suffixes)
                    ; and some bad ones
                    (insert (->Suffix :somename :diesel :bizarro :red))
                    (insert (->Suffix :somename :hybrid :automatic :red))
                    (insert (->Suffix :somename :diesel :manual :yellow)))]
    (time (fire-rules session))))

(run-examples2)
