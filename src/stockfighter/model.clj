(ns stockfighter.model
  (:require [stockfighter.api :as api]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(def portfolio (atom {}))

(def orders (chan))

(def fills (atom {}))
;; {:symbol {:buy {:ts {:price :qty}}}
;;          {:sell {:ts {:price :qty}}}

(defn add-fills
  "Checks the open orders and add any fills to the fills atom"
  [order]
  (let [check-resp  (api/check-order (:id order))
        fills (:fills check-resp)
        symbol (keyword (:symbol order))
        direction (keyword (:direction order))]
    (if 
        (not (empty? fills)) 
      (do (println "fills: " fills) 
          (map (fn [fill] (do (println fill) (let [_ (println "fill " fill)
                                                   ts (keyword (:ts fill))
                                                   price (:price fill)
                                                   qty (:qty fill)
                                                   new-fill {direction {ts {:price price :qty qty}}}
                                                   _ (println "New Fill: " new-fill)] 
                                               (swap! fills merge new-fill)))) fills)
          (add-fills-to-portfolio ""))
      ))) 

(defn add-fills-to-portfolio
  [id]
  (let [order-status (api/check-order id)]
    (println "Not yet implemented.")
    (comment (if (some (:fills order-status)) (swap! portfolio  ) ))))


(defn start-level
  [name]
  (do (api/start-level name)
      (swap! portfolio merge {:account (:account @api/level-data)})
      (go (add-fills (<! orders)))
      )
  )

(defn add-order
  "Takes an order and adds it to the order channel"
  [order]
 ;; (a/go (a/<! order) (a/>! order))
;;  (clojure.pprint/pprint order)
  (do (swap! orders assoc (:id order) order)
      (>!! orders order)))


(defn order-stock
  [order]
  (let [order-resp (api/order-stock order)]
    (if (not (nil? order-resp)) (add-order order-resp))))

