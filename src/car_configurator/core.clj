(ns car-configurator.core
  (:require [schema.core :as s]))

; NO CODE YET ... Just data

; sample engines
(def diesel-engine {:name "Diesel Engine" :fuel :diesel :volume 1.4 :bhp 100})
(def petrol-engine {:name "Petrol Engine" :fuel :petrol :volume 1.6 :bhp 120})

; sample colours
(def standard-grey {:name "Standard Grey" :rgb {:red 142 :green 142 :blue 142} :hex 0x8E8E8E})
(def sunrise-red {:name "Sunrise Red" :rgb {:red 255 :green 48 :blue 48} :hex 0xFF3030})

; base model
(def base-diesel {:name "Base Model Diesel" :engine diesel-engine :colour standard-grey})

; sample options
(def sports-option {:name "Sports Petrol" :engine petrol-engine :colour sunrise-red})

(def diesel-sports-option {:name       "Sports Diesel"
                           :colour     sunrise-red
                           :mapping-fn (fn [model option]
                                         (println "In diesel-sports-option merge")
                                         (merge model option))}) ; OK, we sneak in a small anonymous function

; CODE
(defn configure
  "Merge the option map with a model map using merge or a function provided by the option itself"
  [model option]
  (if-let [mapping-fn (:mapping-fn option)]
    (mapping-fn model option)
    (merge model option)))

; Run the code
(def config1 (configure base-diesel sports-option))
(def config2 (configure base-diesel diesel-sports-option))

; Print the results
(clojure.pprint/pprint base-diesel)
(clojure.pprint/pprint config1)
(clojure.pprint/pprint config2)

; Add a schema to check the shape of the data - entirely optional
; can be added as you go along or after the fact

(def fuel (s/enum :diesel :petrol :hybrid))
(def engine {:name s/Str :fuel fuel :volume s/Num :bhp s/Int}) ; c02, etc.
(def colour {:name s/Str :rgb {:red s/Int :green s/Int :blue s/Int} :hex s/Int}) ; metallic, etc.
(def model {(s/required-key :name)   s/Str
            (s/required-key :engine) engine
            (s/required-key :colour) colour})

(def option {(s/required-key :name)       s/Str
             (s/optional-key :engine)     engine
             (s/optional-key :colour)     colour
             (s/optional-key :mapping-fn) (s/=> s/Any s/Any)}) ; Schema Limitation: s/Any as we cannot address the types

; Run some checks on the schema
; Can be added as needed in dev and / or prod to check the data against the schemas

(s/validate engine diesel-engine)
(s/validate engine petrol-engine)

(s/validate colour standard-grey)
(s/validate colour sunrise-red)

(s/validate model base-diesel)

(s/validate option sports-option)
(s/validate option diesel-sports-option)

; we can also run this in production rather than the raw function
(s/defn configurator [model :- model option :- option]
  (configure model option))

(s/with-fn-validation (configurator base-diesel sports-option))

