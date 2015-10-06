(ns car-configurator.min-parser
  (:refer-clojure :exclude [==])
  (:require [clara.rules.accumulators :as acc]
            [clara.rules :refer :all]))

(defrecord InvalidOption [field message])

(defrecord Suffix [model fuel transmission colour])

; this is what could come from a DB
(def somename-suffixes [{:model :somename :fuel :diesel :transmission :automatic :colour :black}
                        {:model :somename :fuel :diesel :transmission :manual :colour :black}
                        {:model :somename :fuel :petrol :transmission :manual :colour :black}
                        {:model :somename :fuel :diesel :transmission :automatic :colour :red}
                        {:model :somename :fuel :diesel :transmission :manual :colour :red}])

(def possible-fuels (set (map #(:fuel %) somename-suffixes)))
(def possible-colours (set (map #(:colour %) somename-suffixes)))
(def possible-transmissions (set (map #(:transmission %) somename-suffixes)))

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

; base data as 'good' data into the fact space
(def suffixes (map #(->Suffix (:model %) (:fuel %) (:transmission %) (:colour %)) somename-suffixes))

(defn run-examples []
  (let [rules '[; Rules structure must be quoted so Clara can evaluate it...
                {:name "valid-fuel"
                 :lhs  [{:fact-binding :?suffix
                         :type         car_configurator.min_parser.Suffix
                         :constraints  [(== ?model model)
                                        (== ?fuel fuel)
                                        (not (contains? car-configurator.min-parser/possible-fuels ?fuel))]}]
                 :rhs  (println "fail on" ?fuel "for model" ?model "on suffix" ?suffix)}
                ]
        session (-> (mk-session rules)
                    (insert-all suffixes)
                    ; and some bad ones
                    (insert (->Suffix :somename :diesel :bizarro :red))
                    (insert (->Suffix :somename :hybrid :automatic :red))
                    (insert (->Suffix :somename :diesel :manual :yellow)))]
    (time (fire-rules session))))

(run-examples)

; and this seems nicer

