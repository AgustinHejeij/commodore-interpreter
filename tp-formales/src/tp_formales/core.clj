(ns tp-formales.core
  (:gen-class))

(ns tp-formales.core
  (:require [tp-formales.main :as main]))

(defn -main

  "Commodore interpreter"

  [& args]

  (main/driver-loop))

; A COMPLETAR
; READ, DATA?, RESTORE, CLEAR, LET, LIST, NEW, END, 
; ATN, INT, SIN, EXP, LOG, ASC, *, >, <, >=, <=, <>, OR

; desambiguar-mid desambiguar-mas-menos calcular-expresion

; INPUT