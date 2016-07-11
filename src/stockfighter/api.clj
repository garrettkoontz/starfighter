(ns stockfighter.api
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [stockfighter.market])
  (:import [stockfighter.market Order])
  )

(def level-data (atom {}))

(def url "https://api.stockfighter.io/ob/api")

(def gm-url "https://www.stockfighter.io/gm")

(def auth "e44d5ff421d5b77cb7395b24655d39c723a4d5d3")


(defn with-auth-header
  [fn addr map  & else]
;;  (println addr)
  (fn addr (if (:headers map) 
           (assoc (:headers map) "X-Starfighter-Authorization" auth) 
           (assoc map :headers {"X-Starfighter-Authorization" auth})) 
         else )
  )

(defn heart-beat
  []
  (:ok (json/read-str (:body (with-auth-header client/get url {})))))

(defn handle-new-level!
  [body]
      (swap! level-data merge 
             (reduce 
              (fn [mp vl] (assoc mp vl (vl body))) {}
              [:instanceId :account :tickers :venues :secondsPerTradingDay :request-time]))
      )

(defn start-level
  [name]
  (let [body (json/read-str
              (:body
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
     (json/read-str
      (:body
       (with-auth-header client/get 
         (str (exchange-and-stock exchange stock) "/quote") {})) :key-fn keyword)))

(defn get-order-book
  ([]
   (get-order-book (->> @level-data :venues first) (->> @level-data :tickers first)))
  ([exchange stock]
     (json/read-str
      (:body (with-auth-header client/get 
               (exchange-and-stock exchange stock) {})) :key-fn keyword)))


(defn check-order
  "Checks the order at the exchange. If no order (or a nil order number) is provided, returns all orders."
  ([]
   (check-order nil))
  ([id]
      (let 
          [check-resp
           (json/read-str (:body (try (with-auth-header 
                                        client/get
                                        (str url "/venues/" (->>@level-data :venues first)
                                             (if (nil? id)
                                               (str "/accounts/" (->> @level-data :account) "/orders")
                                               (str "/stocks/" (->> @level-data :tickers first) "/orders/" id))
                                             )                              
                                        {}) (catch Exception e {:body (str "{" "\"caught-exception\":\"" (.getMessage e) "\" \"ok\": false" "}")}) )) :key-fn keyword) 
           ]
        (if (:ok check-resp) check-resp (:caught-exception check-resp)))))

(defn order-stock
  "Takes an order record and sends it to the exchange. Calls add-order if successful or prints error message if not."
  ([qty price side]
     (println "Not yet implemented"))
  ([order]
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
                             
                             ) 
                           ) :key-fn keyword)]
       (if (:ok order-resp) order-resp (println (str "Error: " order-resp))))))


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



