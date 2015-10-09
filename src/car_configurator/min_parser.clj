(ns car-configurator.min-parser
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

(def valid-fuels (set (map #(:fuel %) partial-configurations)))
(def valid-colours (set (map #(:colour %) partial-configurations)))
(def valid-transmissions (set (map #(:transmission %) partial-configurations)))


(defrule check-valid-fuel
         "Inserts an InvalidOption fact if the model presents an invalid fuel."
         [?fuel-count <- (acc/distinct :fuel) :from [PartiallyConfiguredCar]]
         [:test (not (empty? (set/difference ?fuel-count valid-fuels)))]
         =>
         (println "fails on" (set/difference ?fuel-count valid-fuels)))

(defn run-examples []
  (let [session (-> (mk-session 'car-configurator.min-parser)
                    (insert-all suffixes)
                    (insert-all valid-fuels)
                    ; and a bad one
                    (insert (->PartiallyConfiguredCar :somename :jibberish :manual :red)))]
    (comment time (fire-rules session))))

;(run-examples)

(defn valid-option-accum [key]
  (acc/distinct key))

(defn valid-option-partial-eq [collection]
  (partial = collection))

(defn partial-invalid-option [key]
  (partial ->InvalidOption key))

(defn partial-diff [collection]
  (let [func (fn [valid-range presented-range]
               (set/difference presented-range valid-range))]
    (partial func collection)))

(defn partial-validity-check [collection]
  (let [func (fn [valid-range presented-range] (not (= valid-range presented-range)))]
    (partial func collection)))

(def partial-colours-diff (partial-diff valid-colours))

(def partial-transmission-diff (partial-diff valid-transmissions))

(def partial-colour-validity (partial-validity-check valid-colours))

(def partial-transmission-validity (partial-validity-check valid-transmissions))

; TODO.... now generate collections, rules bindings and RHS to create InvalidOption
; constraints and rules are all generated!!

;(def valid-fuels (set (map #(:fuel %) somename-suffixes)))

(def fuel-accum (valid-option-accum :fuel))

(def partial-eq-fuels (valid-option-partial-eq valid-fuels))

(def partial-eq-colour (valid-option-partial-eq valid-colours))


; and now use what we know to generate the rules...
(defn derive-valid-option [training-data option]
  (set (map #(option %) training-data)))


;(def map-of-options "")
;(def accum-rule-template {:lhs [{:result-binding (keyword "?result")
;                                 :accumulator    (valid-option-accum "<<key>>")
;                                 :from           {:type        car_configurator.min_parser.PartiallyConfiguredCar
;                                                  :constraints []}}
;                                {:constraints [((partial-validity-check "<<collection>>") (symbol "?result"))]}]
;                          :rhs (partial-invalid-option (str "Fails on key: " "<<key>>" ((partial-diff "<<collection>>") (symbol "?result"))))})


(defn gen-rule [type training-data option-to-check]
  (let [valid-options (derive-valid-option training-data option-to-check)
        result-binding-keyword (keyword "?result")
        accumulator (valid-option-accum option-to-check)
        from {:type type :constraints []}
        constraints (vector '(partial-validity-check valid-options ?result))
        rhs '(println (str "Fails on value: " ((partial-diff valid-options) ?result)))
        ]
    {:lhs [{:result-binding result-binding-keyword
            :accumulator    accumulator
            :from           from}
           {:constraints constraints}]
     :rhs rhs}))

(def options-to-check #{:fuel :transmission :colour})

(def typeToValidate car_configurator.min_parser.PartiallyConfiguredCar)

(def fuel-generated-rule
  (gen-rule typeToValidate partial-configurations :fuel))

(defn gen-rules [training-data options-to-check]
  (map #(gen-rule typeToValidate training-data %) options-to-check))

(def colour-rule-as-data
  '{:lhs [{:result-binding :?presented-options
           :accumulator    (acc/distinct :colour)
           :from           {:type        car_configurator.min_parser.PartiallyConfiguredCar
                            :constraints []}}
          {:constraints [(partial-colour-validity ?presented-options)]}]
    :rhs (println "Fails on colours all partial everywhere:" (partial-colours-diff ?presented-options))})

(def transmission-rule-as-data
  '{:lhs [{:result-binding :?presented-options
           :accumulator    (acc/distinct :transmission)
           :from           {:type        car_configurator.min_parser.PartiallyConfiguredCar
                            :constraints []}}
          {:constraints [(partial-transmission-validity ?presented-options)]}]
    :rhs (println "Fails on simplified partial diffz:" (partial-transmission-diff ?presented-options))})

(def rules (vector colour-rule-as-data transmission-rule-as-data))

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

(comment defn run-examples2 []
         (let
           [rules '[; Rules structure must be quoted so Clara can evaluate it...

                    fuel-generated-rule

                    ;{:lhs [{:result-binding :?presented-options
                    ;        :accumulator    fuel-accum
                    ;        :from           {:type        car_configurator.min_parser.PartiallyConfiguredCar
                    ;                         :constraints []}}
                    ;       {:constraints [(not (partial-eq-fuels ?presented-options))]}]
                    ; :rhs (partial-invalid-option "fail fast")}

                    {:lhs [{:result-binding :?presented-options
                            :accumulator    (acc/distinct :colour)
                            :from           {:type        car_configurator.min_parser.PartiallyConfiguredCar
                                             :constraints []}}
                           {:constraints [(partial-colour-validity ?presented-options)]}]
                     :rhs (println "Fails on colours all partial everywhere:" (partial-colours-diff ?presented-options))}

                    {:lhs [{:result-binding :?presented-options
                            :accumulator    (acc/distinct :transmission)
                            :from           {:type        car_configurator.min_parser.PartiallyConfiguredCar
                                             :constraints []}}
                           {:constraints [(partial-transmission-validity ?presented-options)]}]
                     :rhs (println "Fails on simplified partial diffz:" (partial-transmission-diff ?presented-options))}
                    ]
            session (-> (mk-session rules)
                        (insert-all suffixes)
                        ; and some bad ones
                        (insert (->PartiallyConfiguredCar :somename :diesel :bizarro :red))
                        (insert (->PartiallyConfiguredCar :somename :hybrid :automatic :red))
                        (insert (->PartiallyConfiguredCar :somename :diesel :manual :yellow)))]
           (time (fire-rules session))))

;(run-examples2)
