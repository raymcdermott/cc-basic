(ns car-configurator.kata-parser)


; so let's assume we have a katashiki string
; each place implies another value
; sometimes the position of one value affects other values

; in clojure a String is a vector so it's easy to index

(def kata-dictionary {:model-code            {:start-pos 0 :end-pos 2}
                      :engine                {:start-pos 3 :end-pos 9}
                      :number-of-cyclinders  {:start-pos 9 :end-pos 10}
                      :cyclinder-arrangement {:start-pos 10 :end-pos 20}})

(defn value [string start end]
  (if (and (> start 0) (>= (count string) end))
    (subs string start end)
    (println "start" start "or" end "is out of bounds for" string)))

(defn getSemanticValue [kata key]
  {:pre []}
  (if (and (keyword? key) (contains? kata-dictionary key))
    (if-let [key (get kata-dictionary key)]
      (let [start (:start-pos key) end (:end-pos key)]
        (value kata start end)))
    (println "cannot find" key "in the kata definition")))

(def sample-kata "32D1GR-FE6V")


(println (getSemanticValue sample-kata :model-code))
(println (getSemanticValue sample-kata :engine))
(println (getSemanticValue sample-kata :cyclinder-arrangement))
(println (getSemanticValue sample-kata :number-of-cyclinders))
(println (getSemanticValue sample-kata :foo))


