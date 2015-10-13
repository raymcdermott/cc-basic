(ns car-configurator.suffix-parser
  (:refer-clojure :exclude [==])
  (:require [clara.rules.accumulators :as acc]
            [clara.rules :refer :all]))

; get all the models / ssn / kata combinations

; get the suffixes for each model

; get the equipment for each suffix (configurable bill of materials)

; ok so now we have the configurable bill of materials for each suffix ...

; can we use Clara to detect the truth about the truth for each piece of
; equipment across the suffixes for this model?

; Clara accumulators are for testing collections

(defrecord ValidationError [field message])

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

; also add it as 'good' data into the fact space
(def suffixes (map #(->Suffix (:model %) (:fuel %) (:transmission %) (:colour %)) somename-suffixes))

; if this is a pattern, maybe myFirstMacro ;-) ... see above
(defrule valid-fuel
         "Transmissions must be in the allowed set"
         [?suffix <- Suffix (== ?model model) (== ?fuel fuel)]
         [:test (not (contains? possible-fuels ?fuel))]
         =>
         (println "fail on" ?fuel "for model" ?model "on suffix" ?suffix))

(defrule valid-colours
         "Transmissions must be in the allowed set"
         [?suffix <- Suffix (not (contains? possible-colours colour))
          (== ?model model)
          (== ?colour colour)]
         =>
         (println "fail on" ?colour "for model" ?model "on suffix" ?suffix))

(defrule valid-transmission
         "Transmissions must be in the allowed set"
         [?suffix <- Suffix (not (contains? possible-transmissions transmission))
          (== ?model model)
          (== ?transmission transmission)]
         =>
         (println "fail on" ?transmission "for model" ?model "on suffix" ?suffix))

(defn run-examples []
  (let [session (-> (mk-session 'car-configurator.suffix-parser)
                    (insert-all suffixes)
                    ; and some bad ones
                    (insert (->Suffix :somename :diesel :bizarro :red))
                    (insert (->Suffix :somename :hybrid :automatic :red))
                    (insert (->Suffix :somename :diesel :manual :yellow))
                    )]
    (-> session ((time fire-rules)))))

(run-examples)

; and this seems nicer

