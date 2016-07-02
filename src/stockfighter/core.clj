(ns stockfighter.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [stockfighter.market]
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



(defn handle-new-level
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
      (handle-new-level body)
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
  [portfolio order]
  (println order)
  (println portfolio))

(defn order-stock
  "Takes an order record and executes it."
  [order portfolio]
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
                           :key-fn keyword
                           }
                          ))
                       )]
    (if (:id order-resp) (add-order order-resp) (:error order-resp))))

(defn -main
  "Takes a level name and starts it."
  [& args]
  (start-level (first args))  )
