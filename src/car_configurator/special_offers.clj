(ns car-configurator.special-offers
  (:refer-clojure :exclude [==])
  (:require [clojure.set :as set]
            [clara.rules.accumulators :as acc]
            [clara.rules :refer :all]))

(defrecord InvalidOption [field message])

(defrecord ValidFuel [fuel])

(defrecord PartiallyConfiguredCar [model fuel transmission colour])

; this is what could come from a DB
(def partial-configurations [{:model :somename :fuel :diesel :transmission :automatic :colour :black}
                             {:model :somename :fuel :diesel :transmission :manual :colour :black}
                             {:model :somename :fuel :petrol :transmission :manual :colour :black}
                             {:model :somename :fuel :diesel :transmission :automatic :colour :red}
                             {:model :somename :fuel :diesel :transmission :manual :colour :red}])

; base data as 'good' data into the fact space
(def suffixes (map #(->PartiallyConfiguredCar (:model %)
                                              (:fuel %)
                                              (:transmission %)
                                              (:colour %)) partial-configurations))

; base functions
(defn partial-diff [collection]
  (let [func (fn [valid-range presented-range]
               (set/difference presented-range valid-range))]
    (partial func collection)))

(defn partial-validity-check [collection]
  (let [func (fn [valid-range presented-range]
               (not (= valid-range presented-range)))]
    (partial func collection)))

; so now map to produce partials programmatically
(defn partials-map [key]
  (let [coll (set (map #(key %) partial-configurations))
        differ (partial-diff coll)
        validator (partial-validity-check coll)]
    {:key       key
     :differ    differ
     :validator validator}))

(defn generate-partials [keys]
  (set (map #(partials-map %) keys)))

(def validity-keys #{:fuel :colour :transmission})

(def partials (generate-partials validity-keys))

(defn partial-finder [partial-key validity-key]
  (partial-key (first (set/select #(= (:key %) validity-key) partials))))

; the partial functions are used within the data to implement the constraints

; with a little guile these rules could be generated rather than written

(def fuel-rule-as-data
  '{:lhs [{:result-binding :?presented-options
           :accumulator    (acc/distinct :fuel)
           :from           {:type        car_configurator.partial_configuration.PartiallyConfiguredCar
                            :constraints []}}
          {:constraints [((partial-finder :validator :fuel) ?presented-options)]}]
    :rhs (println "Fails on fuels all partial everywhere:" ((partial-finder :differ :fuel) ?presented-options))})

(def colour-rule-as-data
  '{:lhs [{:result-binding :?presented-options
           :accumulator    (acc/distinct :colour)
           :from           {:type        car_configurator.partial_configuration.PartiallyConfiguredCar
                            :constraints []}}
          {:constraints [((partial-finder :validator :colour) ?presented-options)]}]
    :rhs (println "Fails on colours all partial everywhere:" ((partial-finder :differ :colour) ?presented-options))})

(def transmission-rule-as-data
  '{:lhs [{:result-binding :?presented-options
           :accumulator    (acc/distinct :transmission)
           :from           {:type        car_configurator.partial_configuration.PartiallyConfiguredCar
                            :constraints []}}
          {:constraints [(set/difference valid-transmissions ?presented-options)]}]
    :rhs (println "Bad config:" (set/difference valid-transmissions ?presented-options))})

(def rules (vector fuel-rule-as-data colour-rule-as-data transmission-rule-as-data))

; now write some standard rules that provide data about special offers
; time of year, colour of car and it's fuel type ... we are keen on selling diesels these days

(defrule diesel-offer
         "We will offer a special offer for black diesels in the month of November 2015"


         )


(defn run-rules-as-data []
  (let [session (-> (mk-session rules)
                    (insert-all suffixes)
                    ; and some bad ones
                    (insert (->PartiallyConfiguredCar :somename :diesel :bizarro :red))
                    (insert (->PartiallyConfiguredCar :somename :hybrid :automatic :red))
                    (insert (->PartiallyConfiguredCar :somename :diesel :manual :yellow))
                    )]
    (fire-rules session)))

(run-rules-as-data)
