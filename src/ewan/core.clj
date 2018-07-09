(ns ewan.core
  (:gen-class))

(def data
  [
   {:rank 2 :provider "BT" :product "more phone calls"}
   {:rank 1 :provider "BT" :product "phone calls"}
   {:rank 5 :provider "sky" :product "loud adverts"}
   {:rank 3 :provider "BT" :product "spam phone calls"}
   {:rank 4 :provider "sky" :product "adverts"}
   {:rank 6 :provider "sky" :product "football"}
   {:rank 7 :provider "BT" :product "loud adverts"}
   ])


(defn ewan
  ([data max-run]
   (ewan (sort-by :rank data) max-run nil 0))
  ([data max-run last-provider last-provider-num]
   (let [[head & rest-data] data
         head-provider      (:provider head)]
     (cond
       (empty? data)                      []
       (not= last-provider head-provider) (cons head (ewan rest-data max-run head-provider 1))
       (< last-provider-num max-run)      (cons head (ewan rest-data max-run head-provider (inc last-provider-num)))
       :break                             (let [[skipped remainder] (split-with #(= (:provider %) last-provider) data)
                                                head                (first remainder)
                                                unused-products     (concat skipped (rest remainder))]
                                            (cons head (ewan unused-products max-run (:provider head) 1)))))))


(clojure.pprint/pprint (ewan data 2))

(clojure.pprint/pprint (sort-by :rank data ))

(defn -main
  [& args]
  (clojure.pprint/pprint (sort-by :rank data ))
  )
