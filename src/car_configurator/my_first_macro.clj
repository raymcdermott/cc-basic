(ns car-configurator.my-first-macro
  (:refer-clojure :exclude [==])
  (:require [clara.rules :refer :all]))


;(defrule valid-<scope>
;         "Transmissions must be in the allowed set"
;         [?suffix <- Suffix (not (contains? possible-<scope>s <scope>))
;          (== ?model model)
;          (== ?<scope> <scope>)]
;         =>
;         (println "fail on" ?<scope> "for model" ?model "on suffix" ?suffix))
;

(defmacro defvalid
  "Generates a rule to remove the boiler plate of testing"
  [scope-name]
  (let [sym-scope# (symbol scope-name)
        possible-scopes# (symbol (str "possible-" scope-name "s"))
        q-scope# (symbol (str "?" scope-name))
        doc# (str "rule generated for " scope-name)]

    `(defrule sym-scope#
              doc#
              [?suffix <- Suffix (not (contains? possible-scopes# ~@scope-name))
               (== ?model model)
               (== q-scope# ~@scope-name)]
              =>
              (println "fail on" q-scope# "for model" ?model "on suffix" ?suffix))))


;(defrule valid-fuel
;         "Transmissions must be in the allowed set"
;         [?suffix <- Suffix (not (contains? possible-fuels fuel))
;          (== ?model model)
;          (== ?fuel fuel)]
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
