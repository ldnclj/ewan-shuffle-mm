(ns ewan.core
  (:require [clojure.pprint :as pprint])
  (:gen-class))

(def data
  [
   {:rank 2 :provider "BT" :product "more phone calls"}
   {:rank 1 :provider "BT" :product "phone calls"}
   {:rank 5 :provider "sky" :product "loud adverts"}
   {:rank 3 :provider "BT" :product "spam phone calls"}
   {:rank 4 :provider "sky" :product "adverts"}
   {:rank 6 :provider "sky" :product "football"}
   {:rank 7 :provider "BT" :product "have you had an accident in the last 3 years that wasn't your fault"}
   {:rank 8 :provider "BT" :product "line rental"}
   ])


(defn ewan
  ([data max-run]
   (ewan (sort-by :rank data) max-run nil 0))
  ([data max-run last-provider used-times]
   (let [[head & rest-data] data
         head-provider      (:provider head)]
     (cond
       (empty? data)                      []
       (not= last-provider head-provider) (cons head (ewan rest-data max-run head-provider 1))
       (< used-times max-run)             (cons head (ewan rest-data max-run head-provider (inc used-times)))
       :break                             (let [[skipped remainder] (split-with #(= (:provider %) last-provider) data)
                                                head                (first remainder)
                                                unused-products     (concat skipped (rest remainder))]
                                            (if  (empty? remainder) skipped
                                                 (cons head (ewan unused-products max-run (:provider head) 1))))))))


;(clojure.pprint/pprint (ewan data 2))
;(clojure.pprint/pprint (sort-by :rank data ))

(defn -main
  [& args]
  (println "old way")
  (pprint/pprint (sort-by :rank data ))

  (println "new way max 2")
  (clojure.pprint/pprint (ewan data 2))
  (println "new way max 1")
  (clojure.pprint/pprint (ewan data 1))
  )
