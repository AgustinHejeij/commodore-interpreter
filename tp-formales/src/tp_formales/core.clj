(ns tp-formales.core
  (:gen-class))

(ns tp-formales.core
  (:require [tp-formales.main :as main]))

(defn -main

  "Commodore interpreter"

  [& args]

  (main/driver-loop))
