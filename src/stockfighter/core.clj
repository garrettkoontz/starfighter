(ns stockfighter.core
  (:require [clj-http.client :as client]))

(defn with-auth-header
  [addr map  & else]
  '(addr (if (map :headers) (assoc (map :headers) "X-Starfighter-Authorization" "e44d5ff421d5b77cb7395b24655d39c723a4d5d3") (assoc map {:headers {"X-Starfighter-Authorization" "e44d5ff421d5b77cb7395b24655d39c723a4d5d3"}})) else )
  )

(defn exchange-and-stock
  [exchange stock]
   "https://api.stockfighter.io/ob/api/venues/" exchange "/stocks/" stock)

(defn get-quote
  [exchange stock]
  (client/get (with-auth-header (str (exchange-stock exchange stock) "/quote"))))



(defn order-stock
  "If no price given, execute market order"
  [account exchange stock qty direction]
  (client/post (with-auth-header (str (exchange-and-stock exchange stock) "/orders")) {"orderType": "market","qty":qty,"direction": direction,"account": account})
  "If price is given but no type, default to limit"
  [account exchange stock qty amount direction]
  ()
  )



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
