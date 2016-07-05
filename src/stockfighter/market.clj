(ns stockfighter.market
  (:gen-class)
  )

(defrecord Stock [symbol])

(defrecord Exchange [name stocks])

(defrecord Portfolio [positions orders])

(defrecord Order [account exchange stock qty price direction type])

