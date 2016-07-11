(ns stockfighter.controller
  (:require
            [stockfighter.model :as model]
            [stockfighter.api :as api]
            [stockfighter.market]
            )
  (:import [stockfighter.market Order]))

(defn start-level 
  [name]
  (model/start-level name))

(defn beat-spread
  ([amount]
   (beat-spread amount (->> @api/level-data :venues first) (->> @api/level-data :tickers first) 50))
  ([amount qty]
     (beat-spread amount (->> @api/level-data :venues first) (->> @api/level-data :tickers first) qty))
  ([amount qty side]
     (beat-spread amount (->> @api/level-data :venues first) (->> @api/level-data :tickers first) qty side))
  ([amount exchange stock qty]
   (let [amt (int  (/ amount 2))]
       (future (beat-spread amt exchange stock qty "buy"))
       (future (beat-spread amt exchange stock qty "sell"))))
  ([amount exchange stock qty side]
     (let [spread (api/get-spread exchange stock)
           {:keys [bid ask]} spread
;;           _ (println (str "spread: " spread))
           new-ask (if (:price ask) (- (:price ask) amount) (+ amount (:price bid)))
           new-bid (if (:price bid) (+ (:price bid) amount) (- (:price ask) amount))]
       (model/order-stock (Order. (:account @api/level-data) 
                            exchange 
                            stock 
                            qty 
                            (cond
                             (= side "buy") new-bid
                             (= side "sell") new-ask)
                            side
                            "limit")))))

(defn buy-order-book
  []
  (let [

         order-book (api/get-order-book)
;;         _ (println order-book)
         exchange (:venue order-book)
         stock (:symbol order-book)
         new-bid (if (not (nil? (:asks order-book))) (apply max (map #(:price %) (:asks order-book))))
         new-ask (if (not (nil? (:bids order-book))) (apply min (map #(:price %) (:bids order-book))))
         total-ask (apply + (map #(:qty %) (:bids order-book)))
         total-bid (apply + (map #(:qty %) (:asks order-book)))
         buy-future (future (model/order-stock
                             ;;println
                             ( Order. (:account @api/level-data) 
                                              exchange 
                                              stock 
                                              total-bid
                                              new-bid
                                              "buy"
                                              "immediate-or-cancel")))
         sell-future (future (model/order-stock
                              ;;println
                              (Order. (:account @api/level-data) 
                                              exchange 
                                              stock 
                                              total-ask
                                              new-ask
                                              "sell"
                                              "immediate-or-cancel")))
        sell-now (future (model/order-stock
                              ;;println
                              (Order. (:account @api/level-data) 
                                              exchange 
                                              stock 
                                              500
                                              75
                                              "sell"
                                              "limit")))
        buy-now (future (model/order-stock
                              ;;println
                              (Order. (:account @api/level-data) 
                                              exchange 
                                              stock 
                                              500
                                              50
                                              "buy"
                                              "limit")))] (api/get-order-book)))
