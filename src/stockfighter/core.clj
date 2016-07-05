(ns stockfighter.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [stockfighter.market]
            [cljs.core.match :refer-macros [match]]
            )
  (:import [stockfighter.market Order]))

(def url "https://api.stockfighter.io/ob/api")

(def gm-url "https://www.stockfighter.io/gm")

(def auth "e44d5ff421d5b77cb7395b24655d39c723a4d5d3")

(def level-data (atom {}))

(defn with-auth-header
  [fn addr map  & else]
  (fn addr (if (:headers map) 
           (assoc (:headers map) "X-Starfighter-Authorization" auth) 
           (assoc map :headers {"X-Starfighter-Authorization" auth})) 
         else )
  )



(defn handle-new-level!
  [body]
      (swap! level-data merge 
             (reduce 
              (fn [mp vl] (assoc mp vl (vl body))) {}
              [:instanceId :account :tickers :venues :secondsPerTradingDay :request-time]))
      
      )

(defn start-level
  [name]
  (let [body  (json/read-str (:body
                                    (with-auth-header 
                                      client/post 
                                      (str gm-url "/levels/" name) 
                                      {})) :key-fn keyword)]
    (if (:ok body)
      (handle-new-level! body)
      (println (str  "level not started! Reason: " body))
      )
    ))

(defn exchange-and-stock
  [exchange stock]
  (str url "/venues/" exchange "/stocks/" stock))

(defn get-quote
  ([]
   (get-quote (->> @level-data :venues first) (->> @level-data :tickers first)))
  ([exchange stock]
   (json/read-str (:body  (with-auth-header client/get 
                            (str (exchange-and-stock exchange stock) "/quote") {})) :key-fn keyword)))

(defn get-order-book
  ([]
   (get-order-book (->> @level-data :venues first) (->> @level-data :tickers first)))
  ([exchange stock]
   (json/read-str (:body (with-auth-header client/get 
                           (exchange-and-stock exchange stock) {})) :key-fn keyword)))

(defn add-order
  [order]
  (println (str "New Order: " order)))

(defn order-stock
  "Takes an order record and executes it."
  [order]
  (let [order-resp 
        (json/read-str (:body 
           (with-auth-header 
             client/post
             (str (exchange-and-stock (:exchange order) (:stock order)) "/orders")
             {:body (json/write-str {:orderType (:type order), 
                                     :qty (:qty order), 
                                     :direction (:direction order), 
                                     :price (:price order), 
                                     :account (:account order)}) 
              }
             
             ) :key-fn keyword
           ))]
    (if (:id order-resp) (add-order order-resp) (println (str "Error: " order-resp)))))

(defn get-spread
  ([]
   (get-spread (->> @level-data :venues first) (->> @level-data :tickers first)))
  ([exchange stock]
   (let [order-book  (get-order-book exchange stock)]
     (get-spread order-book)))
  ([order-book]
   (if (:ok order-book)
     {:bid (->> order-book :bids first)
      :ask (->> order-book :asks first)
      :exchange (:venue order-book)
      :symbol (:symbol order-book)
      :time (:ts order-book)})
     ))

(defn beat-spread
  ([amount]
   (beat-spread amount (->> @level-data :venues first) (->> @level-data :tickers first) 50))
  ([amount qty]
   (beat-spread amount (->> @level-data :venues first) (->> @level-data :tickers first) qty))
  ([amount exchange stock qty]
   (let [amt (/ amount 2)]
     (beat-spread amt exchange stock qty "buy")
     (beat-spread amt exchange stock qty "sell")))
  ([amount exchange stock qty side]
   (let [spread (get-spread exchange stock)
         {:keys [bid ask]} spread
         _ (println spread)
         new-ask (if (:price ask) (- (:price ask) amount) (+ amount (:price bid)))
         new-bid (if (:price bid) (+ (:price bid) amount) (- (:price ask) amount))]
     (order-stock (Order. (:account @level-data) 
                          exchange 
                          stock 
                          qty 
                          (cond
                           (= side "buy") new-bid
                           (= side "sell") new-ask)
                          side
                          "limit")))))


(defn -main
  "Takes a level name and starts it."
  [& args]
  (start-level (first args))  )
