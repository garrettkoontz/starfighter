(ns stockfighter.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [stockfighter.market :as market]
            [stockfighter.model :as model]
            [stockfighter.api :as api]
            [stockfighter.controller :as controller]
))



(defn -main
  "Takes a level name and starts it."
  [& args]
  (controller/start-level (first args))  )
