(ns stockfighter.core
  (:require [clj-http.client :as client]
            [clojure.core.async :as a]
            [clojure.data.json :as json]
            [stockfighter.market]
            [cljs.core.match :refer-macros [match]]
            )
  (:import [stockfighter.market Order]))

(def url "https://api.stockfighter.io/ob/api")

(def gm-url "https://www.stockfighter.io/gm")

(def auth "e44d5ff421d5b77cb7395b24655d39c723a4d5d3")

(def level-data (atom {}))

(def portfolio (atom {}))

(def orders (atom {}))

(def fills (atom {}))

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
      (swap! portfolio merge {:account (:account @level-data)})
      (swap! orders {})
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

(defn add-order
  [order]
 ;; (a/go (a/<! order) (a/>! order))
  (clojure.pprint/pprint order)
  (swap! orders assoc (:id order) order))

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

(defn check-fills!
  [id]
  (map
   (fn [fill] (swap! fills update-in [:])) (:fills (check-order id)) ))

(defn add-fills-to-portfolio
  [id]
  (let [order-status (check-order id)]
    (if (some (:fills order-status)) (swap! portfolio  ) )))

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
       (if (:id order-resp) (add-order order-resp) (println (str "Error: " order-resp))))))

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
  ([amount qty side]
     (beat-spread amount (->> @level-data :venues first) (->> @level-data :tickers first) qty side))
  ([amount exchange stock qty]
     (let [amt (/ amount 2)]
       (beat-spread amt exchange stock qty "buy")
       (beat-spread amt exchange stock qty "sell")))
  ([amount exchange stock qty side]
     (let [spread (get-spread exchange stock)
           {:keys [bid ask]} spread
           _ (println (str "spread: " spread))
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

(defn buy-order-book
  []
  (let [

         order-book (get-order-book)
         _ (println order-book)
         exchange (:venue order-book)
         stock (:symbol order-book)
         new-bid (if (not (nil? (:asks order-book))) (apply max (map #(:price %) (:asks order-book))))
         new-ask (if (not (nil? (:bids order-book))) (apply min (map #(:price %) (:bids order-book))))
         total-ask (apply + (map #(:qty %) (:bids order-book)))
         total-bid (apply + (map #(:qty %) (:asks order-book)))
         buy-future (future (order-stock
                             ;;println
                             ( Order. (:account @level-data) 
                                              exchange 
                                              stock 
                                              total-bid
                                              new-bid
                                              "buy"
                                              "immediate-or-cancel")))
         sell-future (future (order-stock
                              ;;println
                              (Order. (:account @level-data) 
                                              exchange 
                                              stock 
                                              total-ask
                                              new-ask
                                              "sell"
                                              "immediate-or-cancel")))
        sell-now (future (order-stock
                              ;;println
                              (Order. (:account @level-data) 
                                              exchange 
                                              stock 
                                              500
                                              75
                                              "sell"
                                              "limit")))
        buy-now (future (order-stock
                              ;;println
                              (Order. (:account @level-data) 
                                              exchange 
                                              stock 
                                              500
                                              50
                                              "buy"
                                              "limit")))] (get-order-book)))

(defn -main
  "Takes a level name and starts it."
  [& args]
  (start-level (first args))  )
